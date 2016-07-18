open Nodejs_high_level

let () =
  Fs.read_file_sync "example.json" Fs.Read |> print_endline;

  Fs.read_file_async "example.json"
    (fun err contents ->
      contents##toString |> Js.to_string |> print_endline)
