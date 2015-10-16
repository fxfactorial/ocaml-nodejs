

class type net = object

end

let require () : net Js.t =
  Nodejs_globals.require (Js.string "net")
