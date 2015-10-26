open Nodejs_kit

class type net = object

end

let require () : net Js.t = require "net"
