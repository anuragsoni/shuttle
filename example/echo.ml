open! Core
open! Async
open Shuttle

let run port =
  let%bind server =
    Connection.listen
      ~max_accepts_per_batch:64
      ~input_buffer_size:1024
      ~output_buffer_size:1024
      ~on_handler_error:`Raise
      ~f:(fun _client_addr reader writer ->
        let ivar = Ivar.create () in
        let rec loop reader writer ivar =
          Input_channel.refill reader
          >>> function
          | `Eof -> Ivar.fill ivar ()
          | `Ok | `Buffer_is_full ->
            let view = Input_channel.view reader in
            Output_channel.write_bigstring writer view.buf ~pos:view.pos ~len:view.len;
            Output_channel.schedule_flush writer;
            Input_channel.consume reader view.len;
            loop reader writer ivar
        in
        loop reader writer ivar;
        Ivar.read ivar)
      (Tcp.Where_to_listen.of_port port)
  in
  Tcp.Server.close_finished_and_handlers_determined server
;;

let command =
  Command.async
    ~summary:"echo server"
    Command.Let_syntax.(
      let%map_open port = anon ("port" %: int) in
      fun () -> run port)
;;

let () = Command_unix.run command
