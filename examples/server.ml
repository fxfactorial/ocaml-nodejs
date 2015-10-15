

let () =
  let http = Nodejs.Http.require () in
  let our_server =
    http##createServer(Js.wrap_callback
                         (fun a b -> print_endline "Handled request"))
  in
  (* Not working yet *)
  our_server##listen
    8080 (Js.wrap_callback (fun () ->
        let family = (our_server##address ())##.family in
        let a = Js.to_string family in
        print_endline a;
        print_endline "started server"))
