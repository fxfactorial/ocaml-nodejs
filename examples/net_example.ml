
open Nodejs
open Bindings_utils

let () =
  let server = Net.net##createServer_withConnListener
      (Js.wrap_callback (fun c ->

           print_endline "Client connected";

           c##on_withNoArgs
             (Js.string "end")
             (Js.wrap_callback (fun () -> print_endline "Client disconnected"));


           c##write (Buffer.buffer_static##from (Js.string "Hello\r\n"));

           c##pipe c |> ignore;

           ()))
  in
  server##listen 8124 (Js.wrap_callback (fun _ -> print_endline "Server bound"))
