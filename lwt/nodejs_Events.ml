open Js
open Pervasives_Js

let _events =
  require_module "events"

class type eventEmitter =
object
  method emit : Unsafe.any js_array t -> unit meth
  method on : js_string t -> ('a -> 'b) callback -> unit meth
  method addListener : js_string t -> ('a -> 'b) callback -> unit meth
  method once : js_string t -> ('a -> 'b) callback -> unit meth
  method removeListener : js_string t -> Unsafe.any js_array t -> unit meth
  method setMaxListeners : int -> unit meth
  method getMaxListeners : int meth
end

let create_event_emitter : unit -> eventEmitter t =
  fun () -> Unsafe.new_obj _events [| |]

let emit (obj : eventEmitter t) s (argv : Unsafe.any array) : unit =
  Unsafe.meth_call obj "emit"
    (Array.append [| unsafe_string s |] argv)

let add_listener (obj : eventEmitter t) s (f : 'a -> 'b) : unit =
  Unsafe.meth_call obj "addListener"
    [| unsafe_string s; unsafe_callback f |]

let on (obj : eventEmitter t) s (f : 'a -> 'b) : unit =
  Unsafe.meth_call obj "addListener"
    [| unsafe_string s; unsafe_callback f |]

let once (obj : eventEmitter t) s (f : 'a -> 'b) : unit =
  Unsafe.meth_call obj "once"
    [| unsafe_string s; unsafe_callback f |]

let remove_listener (obj : eventEmitter t) s (f : 'a -> 'b) : unit =
  Unsafe.meth_call obj "removeListener"
    [| unsafe_string s; unsafe_callback f|]

let remove_all_listeners (obj : eventEmitter t) s : unit =
  match s with
  | None -> Unsafe.meth_call obj "removeListener" [||]
  | Some event -> Unsafe.meth_call obj "removeListener"
                    [| unsafe_string event |]

let set_max_listeners (obj : eventEmitter t) (n : int) : unit =
  Unsafe.meth_call obj "setMaxListeners" [|Unsafe.inject n;|]

let get_max_listeners (obj : eventEmitter t) : int =
  Unsafe.meth_call obj "getMaxListeners" [||]

let get_default_max_listeners : unit -> int =
  fun () -> Unsafe.get _events "defaultMaxListeners"

let set_default_max_listeners (n : int) =
  Unsafe.set _events "defaultMaxListeners" n

type t = eventEmitter Js.t
