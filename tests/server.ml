open Nodejs

let () =
  Js.Unsafe.meth_call Http.server "listen"
    [|Js.Unsafe.inject Http.port;
      Js.Unsafe.inject (fun _ -> print_endline "server started")|]
