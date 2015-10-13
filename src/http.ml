let port = Js.number_of_float 8080.0

let http : Js.Unsafe.any Js.t =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "require")
    [| Js.Unsafe.inject (Js.string "http") |]

let handle_request (req: Js.Unsafe.any) (resp : Js.Unsafe.any) =
  Js.Unsafe.meth_call resp "end" [|Js.string "It Works!" |> Js.Unsafe.inject|]

let server =
  Js.Unsafe.meth_call http "createServer" [|Js.Unsafe.inject handle_request|]
