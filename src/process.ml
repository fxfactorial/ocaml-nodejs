class type process = object

  method version : Js.js_string Js.t Js.readonly_prop

  method platform : Js.js_string Js.t Js.readonly_prop

end

let process : process Js.t = Js.Unsafe.global##.process
