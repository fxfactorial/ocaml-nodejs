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
  let our_server =
    H.create_server begin fun _ _ ->
      print_endline "hello"
    end
  in

  our_server#listen 8080 (fun () -> print_endline "did this work")
