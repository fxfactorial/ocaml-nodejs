open Nodejs_kit

class type fs = object

  method readFile :
    js_str ->
    ('a Js.t -> js_str -> unit) Js.callback ->
    unit Js.meth

end

let require () : fs Js.t =
 Nodejs_kit.require "fs"
