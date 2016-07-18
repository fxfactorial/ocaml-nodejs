open Lwt.Infix

let () =
  (Nodejs_high_level_lwt.Fs.read_file (Nodejs_high_level.__filename ())
   >>= fun result -> result#to_string |> print_endline |> Lwt.return
   >|= fun () -> print_endline "Finished Program and Ordered Async")
  |> Lwt.ignore_result

