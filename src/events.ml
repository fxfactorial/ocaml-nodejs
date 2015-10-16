class type event = object

end

class type event_emitter = object
  method on :
    Js.js_string Js.t ->
    (Js.Unsafe.any -> unit) Js.callback -> unit Js.meth

  method emit : event Js.t -> bool Js.t Js.meth

  method emit_withargs :
    event Js.t ->
    Js.Unsafe.any Js.js_array Js.t ->
    bool Js.t Js.meth

end

class type events = object

end

let require () : events Js.t =
  Nodejs_globals.require (Js.string "events")
