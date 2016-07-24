let __filename () = Nodejs.__filename () |> Js.to_string

let __dirname () = Nodejs.__dirname () |> Js.to_string


module Buffer = struct

  type exn += Buffer_not_init

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

    method to_string =
      if init_buffer then begin
        let b = Js.Opt.get buffer_handle (fun _ -> assert false) in
        b##toString |> Js.to_string
      end else raise Buffer_not_init

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
      callback err b
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

module Os = struct

  (* Later hide with mli *)
  let os_handle = Nodejs.Os.os

  type cpu_stat = { model: string;
                    speed: int;
                    times: <user:int;
                            nice: int;
                            sys: int;
                            idle: int;
                            irq: int;> }

  type endianess = Big | Little

  type platform = Aix | Darwin | Freebsd | Linux | Openbsd | Sunos | Win32 | Android

  (* Hide as well in mli *)
  let endianess_of_string =
    function "BE" -> Big | "LE" -> Little | _ -> assert false

  let platform_of_string = function
    | "aix" -> Aix
    | "darwin" -> Darwin
    | "freebsd" -> Freebsd
    | "linux" -> Linux
      (* TODO FINISH*)
    | _ -> assert false

  let cpu_stats () =
    os_handle##cpus
    |> Js.to_array
    |> Array.map (fun c ->
        {model = c##.model |> Js.to_string;
         speed = c##.speed;
         times = object
           method user = c##.times##.user
           method nice = c##.times##.nice
           method sys = c##.times##.sys
           method idle = c##.times##.idle
           method irq = c##.times##.irq
         end}
      )
    |> Array.to_list

  let endianess () =
    os_handle##endianness |> Js.to_string |> endianess_of_string

  let free_memory_available () = os_handle##freemem

  let home_directory () = os_handle##homedir |> Js.to_string

  let hostname () = os_handle##hostname |> Js.to_string

  let load_average () = os_handle##loadavg |> Js.to_array

end

class process = object
  val js = Nodejs.process
  method arguments =
    js##.argv |> Js.to_array |> Array.map Js.to_string |> Array.to_list
  method abort = js##abort
  method arch = js##.arch |> Js.to_string
  method change_dir s = js##chdir (Js.string s)
  method config : 'a. 'a Js.t = js##.config
  method connect = js##.connected |> Js.to_bool
  method current_working_directory = js##cwd |> Js.to_string
  method environment_variables : 'b. 'b Js.t = js##.env
  method pid = js##.pid
  method exit e = js##exit e
end

let process = new process
