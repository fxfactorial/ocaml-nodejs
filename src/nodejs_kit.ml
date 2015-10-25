(** Collection of helper functions and aliases for building
    application based on node *)

(** Alias helper *)
type js_str = Js.js_string Js.t

(** Helper to wrap callback, example:
    server##listen port !@(fun () -> print_endline "Server Running") *)
let ( !@ ) f = Js.wrap_callback f

(** Shortcut for OCaml string -> JavaScript string, do !$ "Hello" *)
let ( !$ ) s = Js.string s

let js_object_of_alist a =
  List.map (fun (key, value) -> (key, Js.Unsafe.inject value)) a
  |> Array.of_list
  |> Js.Unsafe.obj

(** Does a require("some_module") *)
let require s =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "require")
    [|Js.Unsafe.inject (Js.string s)|]

(** Same as console.log *)
let log obj = Firebug.console##log obj

(** The current file name *)
let __filename () =
  (Js.Unsafe.eval_string "__filename" : js_str)
  |> Js.to_string

(** The current directory name *)
let __dirname () =
  (Js.Unsafe.eval_string "__dirname" : js_str)
  |> Js.to_string
