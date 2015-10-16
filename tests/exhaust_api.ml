
let test_modules () =
  let _ = Nodejs.Http.require () in
  let _ = Nodejs.Net.require () in
  let _ = Nodejs.Url.require () in
  let _ = Nodejs.Events.require () in
  let _ = Nodejs.Fs.require () in
  print_endline "All Requires passed"

let test_globals () =
  let a = Nodejs.Process.process##.version in
  print_endline (Js.to_string a)

let start_server () =
  let http = Nodejs.Http.require () in
  let our_server =
    http##createServer_callback
      (Js.wrap_callback
         (fun a b -> print_endline "Handled request"))
  in
  our_server##listen
    8080 (Js.wrap_callback (fun () -> print_endline "started server"))

let test_all () =
  test_modules ();
  test_globals ();
  start_server ()

let () = test_all ()
