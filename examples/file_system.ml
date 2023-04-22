open Js_of_ocaml

let () =
  let result =
    Nodejs.Fs.fs##readFileSync (Js.string "example.json")
      (object%js
         val encoding = Js.null

         val flag = Js.string "r"
      end )
  in
  result##toString |> Js.to_string |> print_endline

let () =
  Nodejs.Fs.fs##readFile (Js.string "example.json")
    (Js.wrap_callback (fun _ contents ->
       contents##toString |> Js.to_string |> print_endline ) )
