(* Basically a translation of
   http://arminboss.de/2013/tutorial-how-to-create-a-basic-chat-with-node-js/ *)

let () =
  let module H = (val Nodejs.require Nodejs.Http) in
  let module Fs = (val Nodejs.require Nodejs.Fs) in
  let io = Socket_io.require () in
  let server =
    H.create_server begin fun incoming response ->

      Fs.read_file "./client.html" None begin fun err data ->
        response#write_head 200 [("Content-type", "text/html")];
        response#end_ ~data:(H.String data) ()

      end;
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
      let innard = Nodejs.g data "message" in

      io#sockets#emit
        "message_to_client"
        (Nodejs.i (object%js val message = Nodejs.i innard end))

    end
  end
