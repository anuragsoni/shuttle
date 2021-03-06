(** Alternative to
    {{:https://github.com/janestreet/async_unix/blob/cdd9aba67eec2f30bb3a7a22f92c056742073726/src/writer.mli}
    Async_unix.Writer}, based on the low latency transport in async_rpc. *)

open! Core
open! Async_kernel
open Async_unix

type t [@@deriving sexp_of]

(** [create ?buf_len ?write_timeout fd] creates a new writer.

    The writer doesn't flush automatically and the user is responsible for calling
    [flush], which triggers a write system call if needed. *)
val create : ?buf_len:int -> ?write_timeout:Time_ns.Span.t -> Fd.t -> t

(** [monitor] returns the async monitor used by [Output_channel] for performing all write
    operations.*)
val monitor : t -> Monitor.t

(** [remote_closed] is a deferred that's resolved when the consumer that's reading the
    bytes written to the Output_channel is closed, i.e. the channel has received an EPIPE
    or ECONNRESET when it attempts to perform a write. *)
val remote_closed : t -> unit Deferred.t

val is_closed : t -> bool
val is_open : t -> bool
val close_started : t -> unit Deferred.t
val close_finished : t -> unit Deferred.t

(** [write_bigstring] copies the bigstring into the channel's internal buffer. It is safe
    to modify the bigstring once [write_bigstring] returns. *)
val write_bigstring : t -> ?pos:int -> ?len:int -> Bigstring.t -> unit

val schedule_bigstring : t -> ?pos:int -> ?len:int -> Bigstring.t -> unit
  [@@deprecated
    "schedule_bigstring will be removed in a future release. Use [write_bigstring] \
     instead."]

(** [write] copies the string into the channel's internal buffer. The string will surface
    the next time the writer schedules a write. *)
val write : t -> ?pos:int -> ?len:int -> string -> unit

val write_string : t -> ?pos:int -> ?len:int -> string -> unit
  [@@deprecated "write_string will be removed in a future release. Use [write] instead. "]

val write_char : t -> char -> unit
val writef : t -> ('a, unit, string, unit) format4 -> 'a

(** [close] will close the underlying file descriptor after waiting for the writer to be
    flushed. *)
val close : t -> unit Deferred.t

(** [schedule_flush] will schedule a write system call if one is needed. *)
val schedule_flush : t -> unit

(** [flushed t f] deferred that will get resolved when all prior writes have finished. *)
val flushed : t -> unit Deferred.t

(** [flush] schedules a write system call if one is needed and returns a deferred that
    will get resolved when all prior writes have finished. *)
val flush : t -> unit Deferred.t

val pipe : t -> string Pipe.Writer.t
val of_pipe : Info.t -> string Pipe.Writer.t -> (t * unit Deferred.t) Deferred.t

(** [open_file ?buf_len ?append filename] opens a new file and returns a channel that can
    be used to write content to the file. [buf_len] is an optional input and can be used
    to control the channel's buffer size. *)
val open_file : ?buf_len:int -> ?append:bool -> Filename.t -> t Deferred.t

(** [with_file ?buf_len ?append filename ~f] opens a new file and forwards the channel to
    the user provided callback. Once [f] returns the channel and the underlying file is
    closed. *)
val with_file
  :  ?buf_len:int
  -> ?append:bool
  -> Filename.t
  -> f:(t -> 'a Deferred.t)
  -> 'a Deferred.t
