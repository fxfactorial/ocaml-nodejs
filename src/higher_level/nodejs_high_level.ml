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
