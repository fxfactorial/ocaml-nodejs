open Nodejs_high_level

let () =
  Fs.read_file_sync "example.json" Fs.Read |> print_endline;

  Fs.read_file_async "example.json"
    (fun err contents -> contents#to_string |> print_endline)

let () =
  let e = new Events.event_emmiter in
  e#add_listener "speak" (fun _ -> print_endline "Called on speak event");
  e#event_names |> List.iter print_endline;
  e#emit "speak"

let () =
  let s =
    new Net.server (fun client ->
        client#write "Welcome to the Matrix";
        print_endline "Client connected")
  in

  s#listen 8124 (fun () -> print_endline "Created a server")
