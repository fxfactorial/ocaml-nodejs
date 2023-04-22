open Js_of_ocaml

let () =
  let item = Nodejs.Buffer.buffer_static##from (Js.string "Hello World") in
  Printf.sprintf "Length: %d" item##.length |> print_endline
