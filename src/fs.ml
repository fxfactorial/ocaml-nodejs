class type fs = object
  method readFile :
    Js.js_string Js.t ->
    (Js.Unsafe.any Js.t -> Js.js_string Js.t -> unit) Js.callback ->
    unit Js.meth
end

let require () : fs Js.t =
 Nodejs_globals.require "fs"
