

class type event_emitter = object
  method on :
    Js.js_string Js.t ->
    (Js.Unsafe.any -> unit) Js.callback -> unit Js.meth
end

class type events = object

end

let require () : events Js.t =
  Nodejs_globals.require (Js.string "events")
