open Lwt.Infix

let run t =
  let t =
    (Lwt.catch
       (fun () -> t >>= fun () -> exit 0)
       (fun exn ->
          prerr_string "Exception during Nodejs_Main.run: ";
          prerr_string (Printexc.to_string exn);
          prerr_char '\n';
          flush stderr;
          Printexc.print_backtrace stderr;
          exit 1))
  in
  Lwt.async (fun () -> t)
