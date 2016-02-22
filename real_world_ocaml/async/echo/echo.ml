open Core.Std
open Async.Std

(* Copy data from the reader to the writer, using the provided buffer
 * as scratch space *)
let rec copy_blocks buffer r w =
    Reader.read r buffer
    >>= function
        | `Eof -> return ()
        | `Ok bytes_read ->
            Writer.write w buffer ~len:bytes_read;
            Writer.flushed w
            >>= fun () ->
            copy_blocks buffer r w

(** Starts an echo TCP server. *)
let run () =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 8765)
      (fun _addr r w ->
         let buffer = String.create (64 * 1024) in
         copy_blocks buffer r w)
  in
  ignore host_and_port;;

(* Call [run], and then start the scheduler *)
let () =
    run ();
    never_returns (Scheduler.go ())
