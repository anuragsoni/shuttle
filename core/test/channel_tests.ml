open! Core
open! Async
open! Shuttle

let stdout = Lazy.force Writer.stdout

let write_chunks_to_chan wr chunks =
  Deferred.List.iter ~how:`Sequential chunks ~f:(fun chunk ->
      Output_channel.write wr chunk;
      Output_channel.flush wr)
;;

let%expect_test "create a pipe from an input_channel" =
  Unix.pipe (Info.of_string "test input_channel")
  >>= fun (`Reader reader, `Writer writer) ->
  let rd = Input_channel.create reader in
  let wr = Output_channel.create writer in
  let pipe = Input_channel.pipe rd in
  don't_wait_for
    (write_chunks_to_chan
       wr
       [ "Hello, World!"; " This is a line\nWith some more data as part of this chunk\n" ]
    >>= fun () -> Output_channel.close wr);
  let%map () =
    Pipe.iter_without_pushback pipe ~f:(fun payload -> Writer.write stdout payload)
  in
  [%expect
    {|
    Hello, World! This is a line
    With some more data as part of this chunk |}];
  Writer.writef stdout "Input_channel closed? %b" (Input_channel.is_closed rd);
  [%expect {| Input_channel closed? true |}]
;;

let%expect_test "create a pipe from an output channel" =
  Unix.pipe (Info.of_string "test input_channel")
  >>= fun (`Reader reader, `Writer writer) ->
  let rd = Input_channel.create reader in
  let wr = Output_channel.create writer in
  let pipe_w = Output_channel.pipe wr in
  let pipe_r = Input_channel.pipe rd in
  don't_wait_for
    (Deferred.List.iter
       ~how:`Sequential
       [ "Hello!!"; " This is another chunk.\n"; "Pushing to writer\n" ]
       ~f:(fun msg -> Pipe.write pipe_w msg)
    >>= fun () -> Output_channel.close wr);
  let%map () =
    Pipe.iter_without_pushback pipe_r ~f:(fun msg -> Writer.write stdout msg)
  in
  [%expect {|
    Hello!! This is another chunk.
    Pushing to writer |}]
;;

let%expect_test "create input_channel from pipe" =
  let p = Pipe.of_list [ "hello!"; " this is a chunk.\n"; "Another line!\n" ] in
  Input_channel.of_pipe (Info.of_string "testing") p
  >>= fun rd ->
  let pipe_r = Input_channel.pipe rd in
  let%map () =
    Pipe.iter_without_pushback pipe_r ~f:(fun msg -> Writer.write stdout msg)
  in
  [%expect {|
    hello! this is a chunk.
    Another line! |}]
;;

let%expect_test "create output_channel from pipe" =
  let rd, wr = Pipe.create () in
  let%bind t, _flushed = Output_channel.of_pipe (Info.of_string "testing") wr in
  Output_channel.write t "Hello";
  Output_channel.schedule_flush t;
  Output_channel.write t " World!\n";
  Output_channel.schedule_flush t;
  Output_channel.writef t "Another line using writef %d %b\n" 12 false;
  Output_channel.schedule_flush t;
  don't_wait_for (Output_channel.flushed t >>= fun () -> Output_channel.close t);
  let%map () = Pipe.iter_without_pushback rd ~f:(fun msg -> Writer.write stdout msg) in
  [%expect {|
    Hello World!
    Another line using writef 12 false |}]
;;

let%expect_test "can read lines from channel" =
  Unix.pipe (Info.of_string "test input_channel")
  >>= fun (`Reader reader, `Writer writer) ->
  let rd = Input_channel.create reader in
  let wr = Output_channel.create writer in
  let lines = Input_channel.lines rd in
  Output_channel.write wr "Hello\r\n";
  Output_channel.write wr "World\n";
  Output_channel.write wr "this is a line that doesn't end.";
  Output_channel.write wr " It keeps going";
  Output_channel.schedule_flush wr;
  don't_wait_for (Output_channel.flushed wr >>= fun () -> Output_channel.close wr);
  let%map () =
    Pipe.iter_without_pushback lines ~f:(fun msg -> Writer.write_line stdout msg)
  in
  [%expect {|
    Hello
    World
    this is a line that doesn't end. It keeps going |}]
;;

let%expect_test "can read lines with a small internal buffer" =
  Unix.pipe (Info.of_string "test input_channel")
  >>= fun (`Reader reader, `Writer writer) ->
  let rd = Input_channel.create ~buf_len:16 reader in
  let wr = Output_channel.create writer in
  let lines = Input_channel.lines rd in
  Output_channel.write wr (String.init 32 ~f:(fun _ -> 'a'));
  Output_channel.write wr (String.init 32 ~f:(fun _ -> 'b'));
  Output_channel.write_char wr '\n';
  Output_channel.write wr "Hello\r\n";
  Output_channel.write wr "World\n";
  Output_channel.write wr "this is a line that doesn't end";
  Output_channel.schedule_flush wr;
  don't_wait_for (Output_channel.flushed wr >>= fun () -> Output_channel.close wr);
  let%map () =
    Pipe.iter_without_pushback lines ~f:(fun msg ->
        Writer.writef stdout "%s (%d)\n" msg (String.length msg))
  in
  [%expect
    {|
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb (64)
    Hello (5)
    World (5)
    this is a line that doesn't end (31) |}]
;;
