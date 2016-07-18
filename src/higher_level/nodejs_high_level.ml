let __filename () = Nodejs.__filename () |> Js.to_string

let __dirname () = Nodejs.__dirname () |> Js.to_string


module Buffer = struct

  class buffer ?(default=1024) fresh_buffer = object

    val mutable buffer_handle : Nodejs.Buffer.buffer Js.t Js.opt = Js.null
    val mutable init_buffer = false

    initializer
      if fresh_buffer
      then buffer_handle <-
          Nodejs.Buffer.buffer_static##alloc default |> Js.Opt.return;
      init_buffer <- true

    method set_handle new_handle =
      buffer_handle <- new_handle |> Js.Opt.return;
      init_buffer <- true

  end

end


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
    let wrapped = fun err given_buffer ->
      let b = new Buffer.buffer false in
      b#set_handle given_buffer;
      callback err given_buffer
    in
    Nodejs.Fs.fs##readFile (Js.string file_name) (Js.wrap_callback wrapped)

end

module Net = struct

  type exn += Socket_not_init

  class socket fresh_socket = object

    val mutable socket_handle : Nodejs.Net.socket Js.t Js.opt = Js.null
    val mutable init_socket = false

    initializer
      if fresh_socket
      then socket_handle <- new%js Nodejs.Net.socket |> Js.Opt.return;
      init_socket <- true

    method set_handle raw_socket =
      socket_handle <- raw_socket |> Js.Opt.return;
      init_socket <- true

    method write data =
      if init_socket then begin
        let c = Js.Opt.get socket_handle (fun _ -> assert false) in
        c##write (Nodejs.Buffer.buffer_static##from (Js.string data))
      end else raise Socket_not_init

  end


  class server listener = object
    val k =
      let wrapped = fun given_socket ->
        let s = new socket false in
        s#set_handle given_socket;
        listener s
      in
      Nodejs.Net.net##createServer_withConnListener (Js.wrap_callback wrapped)
    (* method listen : 'a 'b. int -> ('a -> 'b) -> unit = fun port callback -> *)
      (*   k##listen port (Js.wrap_callback callback) *)
    method listen port callback = k##listen port (Js.wrap_callback callback)

  end

  (* let create_server listener = *)
  (*   Nodejs.Net.net##createServer_withConnListener (Js.wrap_callback listener) *)


end
