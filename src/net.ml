

class type net = object

end

let require () : net Js.t =
  Nodejs_kit.require "net"
