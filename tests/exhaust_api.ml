
let test_modules () =
  let _ = Nodejs.Http.require () in
  let _ = Nodejs.Net.require () in
  let _ = Nodejs.Url.require () in
  let _ = Nodejs.Events.require () in
  print_endline "All Requires passed"

let start_server () =
  let http = Nodejs.Http.require () in
  let our_server =
    http##createServer(Js.wrap_callback
                         (fun a b -> print_endline "Handled request"))
  in
  our_server##listen
    8080 (Js.wrap_callback (fun () -> print_endline "started server"))

let test_all () =
  test_modules ();
  start_server ()

let () = test_all ()
