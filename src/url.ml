class type url = object

end

let require () : url Js.t =
  Nodejs_globals.require "url"
