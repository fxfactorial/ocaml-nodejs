(* Basically a translation of
   http://arminboss.de/2013/tutorial-how-to-create-a-basic-chat-with-node-js/ *)

(*   let headers = js_object_of_alist [("Content-Type", "text/html")] in *)
(*   let server = *)
(*     http##createServer *)
(*       !@ begin *)
(*       fun request response -> *)
(*         (\* fs##readFile !$"./client.html" *\) *)
(*         (\*   !@ (fun error raw_data -> *\) *)
(*         (\*       response##writeHead 200 headers; *\) *)
(*         (\*       response##end_data raw_data) *\) *)
(*         () *)
(*     end *)
(*   in *)
(*   let app = server##listen port *)
(*       !@ begin fun () -> *)
(*       Printf.sprintf *)
(*         "\n\nStarted Server on local host port %d, node version: %s" *)
(*         port *)
(*         Nodejs.Process.version *)
(*       |> print_endline *)
(*     end *)
(*   in *)
(*   let io = io##listen app in *)
(*   (\* Gives back a namespace object *\) *)
(*   io##.sockets##on *)
(*     !$ "connection" *)
(*     (\* And now we get a socket *\) *)
(*     !@ begin fun socket -> *)
(*     socket##on *)
(*       !$ "message_to_server" *)
(*       (\* For which we have some data, its an object *\) *)
(*       !@ begin fun data -> *)
(*       let innard = Js.Unsafe.get data "message" in *)
(*       io##.sockets##emit *)
(*         !$"message_to_client" *)
(*         (js_object_of_alist [("message", innard)]) *)

let () =
  let module H = (val Nodejs.require Nodejs.Http) in
  let module Fs = (val Nodejs.require Nodejs.Fs) in
  let our_server =
    H.create_server begin fun incoming response ->
      print_endline "Got a request";

      (* List.iter (fun (code, message) -> print_endline message) H.status_codes *)

      (* These all work *)
      (* incoming#headers |> Cohttp.Header.iter begin fun key values -> *)
      (*   print_endline key *)
      (* end; *)
      (* print_endline incoming#http_version; *)
      (* incoming#on_close (fun () -> print_endline "Connection closed"); *)
      (* List.iter (fun (key, value) -> print_endline value) incoming#headers; *)
      (* List.iter print_endline incoming#raw_headers; *)
      (* print_endline incoming#headers; *)

      Fs.read_file "./client.html" None begin fun err data ->
        response#write_head 200 [("Content-type", "text/html")];
        response#end_ ~data:(H.String data) ()
      end;

    end
  in
  our_server#listen ~port:8080 begin fun () ->
    Printf.sprintf "Started Server and Running node: %s" (new Nodejs.process#version)
    |> print_endline
  end
