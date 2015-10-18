let program () =
  let express = Express.require () in
  let app = express##call() in
  let server =
    (Nodejs.Http.require ())##createServer_withapp app
  in
  let io = (Socket_io.require ())##call_arg server in
  let port = 8080 in
  server##listen port (Js.wrap_callback (fun () -> print_endline "Server Running"));
  app##use (Js.wrap_callback begin fun req res ->
      print_endline "serving"
    end)

let run p =
  ignore (p ())

let () =
  run program
