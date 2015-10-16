let require (s : Js.js_string Js.t) =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "require") [|Js.Unsafe.inject s|]

let console : Js.Unsafe.any Js.t = Js.Unsafe.global##.console

let __filename : Js.js_string Js.t = Js.Unsafe.global##.__filename

let __dirname : Js.js_string Js.t = Js.Unsafe.global##.__dirname
