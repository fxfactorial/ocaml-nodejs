open Js

exception Error of error t

let _debug =
  ref true

let debug location fmt =
  let open Printf in
  ksprintf
    (fun s -> if !_debug then eprintf "Debug: %s: %s\n%!" location s)
    fmt

let require_module s =
  Unsafe.fun_call
    (Unsafe.js_expr "require")
    [|Unsafe.inject (string s)|]

let unsafe_string s =
  Unsafe.inject (string s)

let unsafe_callback f =
  Unsafe.inject (Unsafe.callback f)

let unsafe_obj_filter lst =
  let rec loop ax = function
    | [] -> List.rev ax
    | Some(member) :: tl -> loop (member :: ax) tl
    | None :: tl -> loop ax tl
  in
  Unsafe.obj(Array.of_list (loop [] lst))

let array_to_js : ('a -> 'b) -> 'a array -> 'b js_array t =
  fun convert b ->
    let a = new%js array_empty in
    for i = 0 to Array.length b - 1 do
      ignore(a##push (convert b.(i)));
    done;
    a

let array_of_js : ('a -> 'b) -> 'a js_array t -> 'b array =
  fun convert a ->
    Array.init
      (a##.length)
      (fun i -> convert (Optdef.get (array_get a i) (fun () -> invalid_arg "array_get")))


let list_of_js convert a =
  Array.to_list (array_of_js convert a)

let list_to_js convert b =
  array_to_js convert (Array.of_list b)

let maybe_convert obj (field, convert) =
  Optdef.case (Unsafe.get obj field)
    (fun () -> None)
    (fun x -> Some(convert x))

let maybe_member f name = function
  | Some(value) -> Some(name, f value)
  | None -> None

let stringify : 'a -> string =
  fun s -> (to_string(Json.output s))

let keys : 'a t -> string list =
  fun obj ->
    Unsafe.fun_call
      (Unsafe.js_expr "Object.keys")
      [|Unsafe.inject obj|]
    |> list_of_js to_string
