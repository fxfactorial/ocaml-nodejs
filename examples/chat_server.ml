

let () =
  let http = Nodejs.Http.require () in
  let port = 8080 in
  let server =
    http##createServer (Js.wrap_callback begin
        fun request response ->
          response##end_ (Js.string "I can't belive this works")
      end
      )
  in
  server##listen 8080
    begin Js.wrap_callback begin fun () ->
        print_endline "Started server!"
      end
    end
