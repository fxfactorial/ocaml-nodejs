(** Raw call for doing require("some_module") *)

module Bindings_utils = struct
  let require_module s =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "require")
      [|Js.Unsafe.inject (Js.string s)|]

  let ( !@ ) f = Js.wrap_callback f
  let ( !! ) o = Js.Unsafe.inject o

  (** Get the field of a JavaScript Object *)
  let ( <!> ) obj field = Js.Unsafe.get obj field

  (** Same as console.log *)
  let log obj = Firebug.console##log obj

  (** The current file name *)
  let __filename () =
    (Js.Unsafe.eval_string "__filename" : Js.js_string Js.t) |> Js.to_string

  (** The current directory name *)
  let __dirname () =
    (Js.Unsafe.eval_string "__dirname" : Js.js_string Js.t) |> Js.to_string

  (** Call method of a JavaScript object *)
  let m = Js.Unsafe.meth_call

  (** Inject something as a JS object, be sure its Js.t already,
      functions seemingly exempt *)
  let i = Js.Unsafe.inject

  (** Turn a JavaScript Object into a string *)
  let stringify o = Js._JSON##stringify o |> Js.to_string

  (** Turn an OCaml string into a JavaScript string *)
  let to_js_str s = Js.string s |> Js.Unsafe.inject

  (** Turn a string into a JSON object *)
  let json_of_string s = Js._JSON##parse (s |> Js.string)

  (** Create a JavaScript Object out of an alist *)
  let obj_of_alist a_l =
    List.map (fun (key, value) -> (key, Js.Unsafe.inject value)) a_l
    |> Array.of_list |> Js.Unsafe.obj

  (** Turn JavaScript string array into OCaml list of string *)
  let to_string_list g =
    g |> Js.str_array |> Js.to_array |> Array.map Js.to_string |> Array.to_list

  (** Turn OCaml list of strings into JavaScript string array *)
  let of_string_list g =
    g |> Array.of_list |> Array.map Js.string |> Js.array

  (** Get all keys of an Object *)
  let keys obj =
    m (Js.Unsafe.variable "Object") "keys" [|obj|]
    |> Js.to_array |> Array.map Js.to_string |> Array.to_list

  (** Call a function for each value of each key in an Object, throw
      away result *)
  let for_each_iter ~f obj =
    keys obj |> List.iter (fun k -> f (Js.Unsafe.get obj k))

  (** Call a function for each value of each key in an Object, keep
      result *)
  let for_each_map ~f obj =
    keys obj |> List.map (fun k -> f k (Js.Unsafe.get obj k))

end

module Buffer = struct

  class type buffer = object
    method length : int Js.readonly_prop
    method toString : Js.js_string Js.t Js.meth
    method toJSON : 'a. 'a Js.t Js.meth
  end

  class type buffer_static = object
    method from_array : Typed_array.uint16Array -> buffer Js.t Js.meth
    method from : Js.js_string Js.t -> buffer Js.t Js.meth
    method from_with_encoding :
      Js.js_string Js.t -> Js.js_string Js.t -> buffer Js.t Js.meth
    method alloc : int -> buffer Js.t Js.meth
    method compare : buffer Js.t -> buffer Js.t -> bool Js.meth
  end

  let buffer_static : buffer_static Js.t = Js.Unsafe.pure_js_expr "Buffer"

end

module Fs = struct

  class type fs = object
    method readFileSync :
      Js.js_string Js.t ->
      <encoding: Js.js_string Js.t Js.Opt.t Js.readonly_prop;
       flag : Js.js_string Js.t Js.readonly_prop> Js.t ->
      Buffer.buffer Js.t Js.meth

    method readFile :
      Js.js_string Js.t ->
      (Js.error Js.t -> Buffer.buffer Js.t -> unit) Js.callback ->
      unit Js.meth

  end

  let fs : fs Js.t = Bindings_utils.require_module "fs"

end

