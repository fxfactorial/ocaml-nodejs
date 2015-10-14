let require (s : Js.js_string Js.t) =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "require") [|Js.Unsafe.inject s|]
