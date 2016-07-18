

let () =
  let e = new%js Nodejs.Events.event_emitter in

  e##addListener
    (Js.string "eventOne")
    (Js.wrap_callback (fun () -> print_endline "Called in the event callback"));

  let b =
    (* Add this to jsoo api itself *)
    e##eventNames |> Js.str_array |> Js.to_array
    |> Array.map Js.to_string |> Array.to_list
  in

  List.iter print_endline b;

  e##emit (Js.string "eventOne")
