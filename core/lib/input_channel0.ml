open Core
open Async_kernel
open Async_unix
module Logger = Log.Make_global ()

let set_nonblock fd = Fd.with_file_descr_exn fd ignore ~nonblocking:true

module View = struct
  type t = Bytebuffer.t

  let buf t =
    let buf = Bytebuffer.unsafe_buf t in
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
  ;;

  let pos t = Bytebuffer.pos t
  let length t = Bytebuffer.length t
  let consume t = Bytebuffer.drop t
end

type t =
  { fd : Fd.t
  ; mutable reading : bool
  ; mutable is_closed : bool
  ; closed : unit Ivar.t
  ; buf : Bytebuffer.t
  }
[@@deriving sexp_of]

let create ?(buf_len = 64 * 1024) fd =
  let buf_len =
    if buf_len > 0
    then buf_len
    else
      raise_s [%message "Reader.create got negative buf_len" (buf_len : int) (fd : Fd.t)]
  in
  set_nonblock fd;
  { fd
  ; reading = false
  ; is_closed = false
  ; closed = Ivar.create ()
  ; buf = Bytebuffer.create buf_len
  }
;;

let is_closed t = t.is_closed
let closed t = Ivar.read t.closed

let close t =
  if not t.is_closed
  then (
    t.is_closed <- true;
    Fd.close t.fd >>> fun () -> Ivar.fill t.closed ());
  closed t
;;

let refill' t =
  Bytebuffer.compact t.buf;
  if Bytebuffer.available_to_write t.buf = 0
  then `Buffer_is_full
  else (
    let result =
      Fd.syscall_result_exn t.fd t.buf (fun fd buf ->
          Bytebuffer.read_assume_fd_is_nonblocking fd buf)
    in
    if Unix.Syscall_result.Int.is_ok result
    then (
      match Unix.Syscall_result.Int.ok_exn result with
      | 0 -> `Eof
      | n ->
        assert (n > 0);
        `Read_some)
    else (
      match Unix.Syscall_result.Int.error_exn result with
      | EAGAIN | EWOULDBLOCK | EINTR -> `Nothing_available
      | EPIPE | ECONNRESET | EHOSTUNREACH | ENETDOWN | ENETRESET | ENETUNREACH | ETIMEDOUT
        -> `Eof
      | error -> raise (Unix.Unix_error (error, "read", ""))))
;;

let view t = t.buf
let ok = return `Ok
let buffer_is_full = return `Buffer_is_full
let eof = return `Eof

let rec refill t =
  match refill' t with
  | `Read_some -> ok
  | `Buffer_is_full -> buffer_is_full
  | `Eof -> eof
  | `Nothing_available ->
    Fd.ready_to t.fd `Read
    >>= (function
    | `Ready -> refill t
    | `Closed -> eof
    | `Bad_fd ->
      raise_s
        [%message "Shuttle.Input_channel.read: bad file descriptor" ~fd:(t.fd : Fd.t)])
;;

let transfer t writer =
  let finished = Ivar.create () in
  upon (Pipe.closed writer) (fun () -> Ivar.fill_if_empty finished ());
  let rec loop () =
    match refill' t with
    | `Eof -> Ivar.fill_if_empty finished ()
    | `Buffer_is_full | `Read_some ->
      let payload = Bytebuffer.Consume.stringo t.buf in
      Pipe.write writer payload >>> fun () -> loop ()
    | `Nothing_available ->
      Fd.ready_to t.fd `Read
      >>> (function
      | `Ready -> loop ()
      | `Bad_fd -> raise_s [%message "Input_channel.pipe: Bad file descriptor" (t : t)]
      | `Closed -> Ivar.fill_if_empty finished ())
  in
  loop ();
  Ivar.read finished
;;

let pipe t =
  let reader, writer = Pipe.create () in
  (transfer t writer >>> fun () -> close t >>> fun () -> Pipe.close writer);
  reader
;;

let drain t = Pipe.drain (pipe t)

let rec read_line_slow t acc =
  if Bytebuffer.length t.buf = 0
  then (
    match%bind refill t with
    | `Eof ->
      (match acc with
      | [] -> eof
      | xs -> return (`Eof_with_unconsumed xs))
    | `Buffer_is_full ->
      assert
        (* We should never reach this branch as only attempt to refill if the buffer is
           empty *)
        false
    | `Ok -> read_line_slow t acc)
  else (
    let idx = Bytebuffer.index t.buf '\n' in
    if idx > -1
    then (
      let buf = Bytebuffer.unsafe_buf t.buf in
      let pos = Bytebuffer.pos t.buf in
      let len = idx in
      if len >= 1 && Char.equal (Bytes.unsafe_get buf (pos + idx - 1)) '\r'
      then (
        let line = Bytes.To_string.sub buf ~pos ~len:(idx - 1) in
        Bytebuffer.drop t.buf (len + 1);
        return (`Ok (line :: acc)))
      else (
        let line = Bytes.To_string.sub buf ~pos ~len in
        Bytebuffer.drop t.buf (len + 1);
        return (`Ok (line :: acc))))
    else (
      let curr = Bytebuffer.Consume.stringo t.buf in
      read_line_slow t (curr :: acc)))
;;

let read_line t =
  let idx = Bytebuffer.index t.buf '\n' in
  if idx > -1
  then (
    let buf = Bytebuffer.unsafe_buf t.buf in
    let pos = Bytebuffer.pos t.buf in
    let len = idx in
    if len >= 1 && Char.equal (Bytes.unsafe_get buf (pos + idx - 1)) '\r'
    then (
      let line = Bytes.To_string.sub buf ~pos ~len:(idx - 1) in
      Bytebuffer.drop t.buf (len + 1);
      return (`Ok line))
    else (
      let line = Bytes.To_string.sub buf ~pos ~len in
      Bytebuffer.drop t.buf (len + 1);
      return (`Ok line)))
  else (
    match%map read_line_slow t [] with
    | `Eof -> `Eof
    | `Eof_with_unconsumed xs -> `Ok (String.concat (List.rev xs))
    | `Ok chunks -> `Ok (String.concat (List.rev chunks)))
;;

let lines t =
  let r, w = Pipe.create () in
  let finished =
    Deferred.create (fun ivar ->
        let rec loop t =
          read_line t
          >>> function
          | `Eof -> Ivar.fill ivar ()
          | `Ok v ->
            if Pipe.is_closed w
            then Ivar.fill ivar ()
            else Pipe.write w v >>> fun () -> loop t
        in
        loop t)
  in
  upon finished (fun () -> close t >>> fun () -> Pipe.close w);
  r
;;

let rec read t len =
  let view = view t in
  if View.length view > 0
  then (
    let to_read = min (View.length view) len in
    let buf = String.sub (View.buf view) ~pos:(View.pos view) ~len:to_read in
    View.consume view to_read;
    return (`Ok buf))
  else
    refill t
    >>= function
    | `Eof -> return `Eof
    | `Ok | `Buffer_is_full -> read t len
;;