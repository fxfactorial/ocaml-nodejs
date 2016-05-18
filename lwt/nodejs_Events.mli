(** Event emitters. *)
open Js

(** The class of event emitters. *)
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

type t = eventEmitter Js.t
(** The type of event emitters. *)

val create_event_emitter : unit -> t
(** Create a fresh event emitter. *)

val emit : t -> string -> Unsafe.any array -> unit
(** [emit obj event details] notify the [event] with mentioned [details]
    to [obj]. *)

val add_listener : t -> string -> ('a -> 'b) -> unit
(** [add_listener obj event callback] register the [callback] to
    receive [event] notifications in [obj]. *)

val on : t -> string -> ('a -> 'b) -> unit
(** [on obj event callback] register the [callback] to receive [event]
    notifications in [obj]. This is totally equivalent to
    [add_listener].  The functionality duplication only mirrors the
    duplication found in NodeJs. *)

val once : t -> string -> ('a -> 'b) -> unit
(** [once obj event callback] register the [callback] to receive the
    next [event] notification in [obj]. *)

val remove_listener : t -> string -> ('a -> 'b) -> unit
(** [remove_listener obj event callback] prevent the [callback] from
    receiving [event] notifications in [obj]. *)

val remove_all_listeners : t -> string option -> unit
(** [remove_all_listeners obj (Some event)] remove all callbacks from
    the notification list for [event] in [obj]. The form
    [remove_all_listeners obj None] remove all callbacks from the
    notification list for all events in [obj]. *)

val set_max_listeners : t -> int -> unit
(** By default event emitters will print a warning if more than 10
    listeners are added for a particular event.  This threshold can be
    changed or removed (0) using this function. *)

val get_max_listeners : t -> int
(** Return the threshold for reporting too many listeners. (See
    [set_max_listeners]. *)

val get_default_max_listeners : unit -> int
(** The default value for the the threshold for reporting too many
    listeners. (See [set_max_listeners]. It applies for newly crearted
    event emitters. *)

val set_default_max_listeners : int -> unit
(** Set the default value for the the threshold for reporting too many
    listeners. (See [set_max_listeners]. It applies for newly crearted
    event emitters. *)
