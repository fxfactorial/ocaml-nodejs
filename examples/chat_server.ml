(* Basically a translation of
   http://arminboss.de/2013/tutorial-how-to-create-a-basic-chat-with-node-js/ *)
open Nodejs

let () =
  let module H = (val require Http) in
  let module Fs = (val require Fs) in
  let io = Socket_io.require () in
  let server =
    H.create_server begin fun incoming response ->

      Fs.read_file ~path:"./client.html" begin fun err data ->
        response#write_head ~status_code:200 [("Content-type", "text/html")];
        response#end_ ~data:(H.String data) ()

      end
    end
  in
  let app = server#listen ~port:8080 begin fun () ->
      Printf.sprintf
        "Started Server and Running node: %s" (new Nodejs.process#version)
      |> print_endline
    end
  in

  let io = io#listen app in
  io#sockets#on_connection begin fun socket ->

    socket#on "message_to_server" begin fun data ->

      io#sockets#emit
        ~event_name:"message_to_client"
        !!(object%js val message = data <!> "message" end)

    end
  end
