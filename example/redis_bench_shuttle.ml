open! Core
open! Async
open Shuttle

let unlink f = Deferred.ignore_m (Monitor.try_with (fun () -> Unix.unlink f))

let run sock =
  let%bind () = unlink sock in
  let%bind host_and_port =
    Connection.listen
      ~input_buffer_size:0x1000
      ~output_buffer_size:0x1000
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_file sock)
      ~f:(fun _addr reader writer ->
        let lines = Input_channel.lines reader in
        Pipe.iter_without_pushback lines ~f:(fun _ ->
            Output_channel.write writer "+PONG\r\n";
            Output_channel.schedule_flush writer))
  in
  ignore (host_and_port : (Socket.Address.Unix.t, string) Tcp.Server.t);
  Deferred.never ()
;;

let () =
  Command.async
    ~summary:"Start an echo server"
    Command.Let_syntax.(
      let%map_open sock = anon ("socket" %: string) in
      fun () -> run sock)
  |> Command_unix.run
;;
