(* Basically a translation of
   http://arminboss.de/2013/tutorial-how-to-create-a-basic-chat-with-node-js/ *)

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
  let s = Socket_io.require () in
  let our_server =
    H.create_server begin fun incoming response ->
      print_endline "Got a request";

      Fs.read_file "./client.html" None begin fun err data ->
        response#write_head 200 [("Content-type", "text/html")];
        response#end_ ~data:(H.String data) ()
      end;

    end
  in
  let simple_server = our_server#listen ~port:8080 begin fun () ->
      Printf.sprintf
        "Started Server and Running node: %s" (new Nodejs.process#version)
      |> print_endline
    end
  in

  let s2 = s##listen simple_server in
  let sks = s2##.sockets in
  (* Nodejs.log s; *)
  Nodejs.log sks;
  (* print_endline sks#name; *)
  (* Nodejs.log sks; *)

  ()
