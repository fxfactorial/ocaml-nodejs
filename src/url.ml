class type url = object

end

let require () : url Js.t =
  Nodejs_kit.require "url"
