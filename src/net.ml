

class type net = object

end

let require () : net Js.t =
  Internal.require (Js.string "net")
