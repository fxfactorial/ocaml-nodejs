open Lwt.Infix

let () =
  (Nodejs_high_level_lwt.Fs.read_file (Nodejs_high_level.__filename ())
   >|= fun result -> result##toString |> Js.to_string |> print_endline |> Lwt.return
   >|= fun () -> print_endline "Finished Program")
  |> Lwt.ignore_result

