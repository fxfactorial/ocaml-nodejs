
(** Helper to wrap callback, example:
    server##listen port !@(fun () -> print_endline "Server Running") *)
let ( !@ ) f = Js.wrap_callback f

(** Shortcut for OCaml string -> JavaScript string, do !$ "Hello" *)
let ( !$ ) s = Js.string s

let js_object_of_alist a =
  List.map (fun (key, value) -> (key, Js.Unsafe.inject value)) a
  |> Array.of_list
  |> Js.Unsafe.obj

module Http = struct
  include Http
end

module Net = struct
  include Net
end

module Url = struct
  include Url
end

module Events = struct
  include Events
end

module Process = struct
  include Process
end

module Fs = struct
  include Fs
end

(** Get the versioning of the current running node runtime *)
let version = (Process.process##.version) |> Js.to_string
