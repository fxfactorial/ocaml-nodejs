
let js_object_of_alist a =
  a
  |> List.map (fun (key, value) -> (key, Js.Unsafe.inject value))
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

(** Get the versioning of the current running node runtime *)
let version = (Process.process##.version) |> Js.to_string

module Fs = struct
  include Fs
end
