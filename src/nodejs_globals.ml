let require s =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "require")
    [|Js.Unsafe.inject (Js.string s)|]

let console : Js.Unsafe.any Js.t = Js.Unsafe.global##.console

let __filename () =
  (Js.Unsafe.eval_string "__filename" : Js.js_string Js.t)
  |> Js.to_string

let __dirname () =
  (Js.Unsafe.eval_string "__dirname" : Js.js_string Js.t)
  |> Js.to_string
