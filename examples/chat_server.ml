

let () =
  let http = Nodejs.Http.require () in
  let fs = Nodejs.Fs.require () in
  let port = 8080 in
  let server =
    http##createServer (Js.wrap_callback begin
        fun request response ->
          fs##readFile (Js.string "examples/client.html")
            (Js.wrap_callback begin fun error raw_data ->
              print_endline (Js.to_string raw_data);
              response##end_ (Js.string "I can't belive this works")
              end)
      end)
  in
  server##listen port
    begin Js.wrap_callback begin fun () ->
        print_endline "Started server!"
      end
    end
