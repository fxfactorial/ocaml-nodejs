open Nodejs_kit

class type process = object

  method version : js_str Js.readonly_prop

  method platform : js_str Js.readonly_prop

end

let process : process Js.t = Js.Unsafe.global##.process

(** The versioning of the current running node runtime *)
let version = process##.version |> Js.to_string
