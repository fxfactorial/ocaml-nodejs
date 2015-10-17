let program () =
  let http = Nodejs.Http.require () in
  let fs = Nodejs.Fs.require () in
  let io = Socket_io.require () in

  let port = 8080 in
  let headers = Nodejs.js_object_of_alist [("Content-Type", "text/html")] in
  let server =
    http##createServer (Js.wrap_callback begin
        fun request response ->
          fs##readFile (Js.string "./client.html")
            (Js.wrap_callback begin fun error raw_data ->
                response##writeHead 200 headers;
                response##end_data raw_data
              end)
      end)
  in
  let app = server##listen port
    begin Js.wrap_callback begin fun () ->
        Printf.sprintf
          "\n\nStarted Server on local host port %d, node version: %s"
          port
          Nodejs.version
        |> print_endline
      end
    end
  in
  let io = io##listen app in
  (* Gives back a namespace object *)
  io##.sockets##on
    (Js.string "connection")
    (* And now we get a socket *)
    (Js.wrap_callback begin fun socket ->
        socket##on
          (Js.string "message_to_server")
          (* For which we have some data, its an object *)
          (Js.wrap_callback begin fun data ->
              let innard = Js.Unsafe.get data "message" in
              io##.sockets##emit
                (Js.string "message_to_client")
                (Nodejs.js_object_of_alist [("message", innard)])
            end)
      end)

let run p =
  ignore (p ())

let () =
  run program
