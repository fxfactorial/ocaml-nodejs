

class type events = object

end

let require () : events Js.t =
  Internal.require (Js.string "events")
