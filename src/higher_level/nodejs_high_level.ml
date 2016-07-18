let __filename () = Nodejs.__filename () |> Js.to_string

let __dirname () = Nodejs.__dirname () |> Js.to_string

module Events = struct

  class event_emmiter = object
    val k = new%js Nodejs.Events.event_emitter
    method event_names =
      k##eventNames
      |> Js.str_array
      |> Js.to_array
      |> Array.map Js.to_string
      |> Array.to_list

    method add_listener : 'a 'b. string -> ('a -> 'b) -> unit = fun event_name callback ->
      k##addListener (Js.string event_name) (Js.wrap_callback callback)

    method emit event_name = k##emit (Js.string event_name)

  end

end

module Fs = struct

  type flag = Read | Write | Read_write

  let string_of_flag = function Read -> "r" | Write -> "w" | Read_write -> "rw"

  let read_file_sync file_name file_flag =
    let result =
      Nodejs.Fs.fs##readFileSync
        (Js.string file_name)
        (object%js
          val encoding = Js.null
          val flag = Js.string (string_of_flag file_flag)
        end)
    in
    result##toString |> Js.to_string

  let read_file_async file_name callback =
    Nodejs.Fs.fs##readFile (Js.string file_name) (Js.wrap_callback callback)

end
