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

let version = (Process.process##.version) |> Js.to_string
