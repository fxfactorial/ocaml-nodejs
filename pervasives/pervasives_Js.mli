(** Utility functions to interact with the JavaScript layer. *)


(** {6 JavaScript errors} *)

exception Error of Js.error Js.t
(** Exception holding a JavaScript error. *)


(** {6 JavaScript modules} *)

val require_module : string -> 'a
(** [require_module name] is the value returned by the JavaScript code
    {i require(name)}.  The result is compatible with all types and
    is usually constrained by a class type when used in a program. *)


(** {6 JavaScript conversions} *)

val unsafe_string : string -> Js.Unsafe.any
(** Convert an OCaml string to a JavaScript value. *)

val unsafe_callback : ('a -> 'b) -> Js.Unsafe.any
(** Convert an OCaml function to a JavaScript value. *)

val array_to_js : ('a -> 'b) -> 'a array -> 'b Js.js_array Js.t
(** [array_to_js conv a] convert the OCaml array [a] to a JavaScript
    array, using the provided function [conv] to convert array items. *)

val array_of_js : ('a -> 'b) -> 'a Js.js_array Js.t -> 'b array
(** [array_of_js conv a] convert the JavaScript array [a] to an OCaml
    array, using the provided function [conv] to convert array items. *)

val list_to_js : ('a -> 'b) -> 'a list -> 'b Js.js_array Js.t
(** [array_to_js conv a] convert the OCaml list [a] to a JavaScript
    array, using the provided function [conv] to convert array items. *)

val list_of_js : ('a -> 'b) -> 'a Js.js_array Js.t -> 'b list
(** [array_of_js conv a] convert the JavaScript array [a] to an OCaml
    list, using the provided function [conv] to convert array items. *)

val maybe_convert : 'a -> string * ('b -> 'c) -> 'c option
(** [maybe_convert obj (fieldname, convert)] return [Some(convert x)]
    if [x] is the value of the field [fieldname] in [obj] or [None] if the
    field is not defined. *)

(** {6 Javascript Introspection} *)

val keys : 'a Js.t -> string list
(** Return the attribute list of a JavaScript object. *)

(** {6 Debugging JavaScript} *)

val stringify : 'a -> string
(** Stringify any value using JSON.  This is intended as a cheap
    debugging utility function rather than a serialisation
    facility. *)

val debug : string -> ('a, unit, string, unit) format4 -> 'a
(** [debug location fmt] report debugging information in a printf-style. *)
