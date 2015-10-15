class type url = object

end

let require () : url Js.t =
  Internal.require (Js.string "url")
