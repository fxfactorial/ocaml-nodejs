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

  let debug thing field =
  Firebug.console##log (m (thing <!> field) "toString" [||]);

end

type error_arg = Js.error Js.opt

let __filename () = (Js.Unsafe.eval_string "__filename" : Js.js_string Js.t)

let __dirname () = (Js.Unsafe.eval_string "__dirname" : Js.js_string Js.t)

module Events = struct

  class type event_emitter = object('self)
    (* method on : Js.js_string Js.t -> 'a Js.callback -> unit Js.meth *)
    method on_withNoArgs :
      Js.js_string Js.t ->
      (unit -> unit) Js.callback ->
      unit Js.meth
    method on_WithOneArg :
      Js.js_string Js.t ->
      (Js.Unsafe.any Js.t -> unit) Js.callback ->
      unit Js.meth
    method on_WithTwoArg :
      Js.js_string Js.t ->
      (Js.Unsafe.any Js.t -> Js.Unsafe.any Js.t -> unit) Js.callback ->
      unit Js.meth
    method emit : Js.js_string Js.t -> unit Js.meth
    method emit_withArg : Js.js_string Js.t -> 'a Js.t -> unit Js.meth
    method emit_withTwoArgs :
      Js.js_string Js.t -> 'a Js.t -> 'b Js.t -> unit Js.meth
    method getMaxListeners : int Js.meth

    method once : Js.js_string Js.t -> 'a Js.callback -> unit Js.meth

    method addListener : Js.js_string Js.t -> 'a Js.callback -> unit Js.meth

    method eventNames : Js.string_array Js.t Js.meth

  end

  let event_emitter : event_emitter Js.t Js.constr =
    Bindings_utils.require_module "events"

end

module Buffer = struct

  class type buffer = object
    method length : int Js.readonly_prop
    method toString : Js.js_string Js.t Js.meth
    method toJSON : 'a. 'a Js.t Js.meth
    method swap16 : buffer Js.t Js.meth
    method swap32 : buffer Js.t Js.meth

  end

  class type buffer_static = object
    method from_array : Typed_array.uint16Array -> buffer Js.t Js.meth
    method from : Js.js_string Js.t -> buffer Js.t Js.meth
    method from_withEncoding :
      Js.js_string Js.t -> Js.js_string Js.t -> buffer Js.t Js.meth
    method alloc : int -> buffer Js.t Js.meth
    method compare : buffer Js.t -> buffer Js.t -> bool Js.t Js.meth
    method isBuffer : 'a Js.t -> bool Js.t Js.meth
    method isEncoding : Js.js_string Js.t -> bool Js.t Js.meth
  end

  let buffer_static : buffer_static Js.t = Js.Unsafe.pure_js_expr "Buffer"

end

module Stream = struct

  class type stream = object('self)
    inherit Events.event_emitter
  end

  class type writable_stream = object('self)
    inherit stream
    (* method cork *)
    (* method end_ *)
    (* method setDefaultEncoding *)
    (* method uncork *)
      (* method write *)
  end

  class type readable_stream = object('self)
    inherit stream
    method isPaused : bool Js.t Js.meth
    method pause : readable_stream Js.t Js.meth
    method pipe : 'self Js.t -> writable_stream Js.t Js.meth

    (* method pipe_with_options : *)
    (*   writable_stream Js.t -> *)
    (*   #stream Js.t Js.meth *)

    method read : 'a Js.t Js.opt Js.meth
    method read_withAmount : int -> 'a Js.t Js.opt Js.meth
    method resume : readable_stream Js.t Js.meth
    method setEncoding : Js.js_string Js.t -> readable_stream Js.t Js.meth
    method unpipe : unit Js.meth
    method unpipe_withDest : writable_stream Js.t -> unit Js.meth
    method unshift : Buffer.buffer Js.t -> unit Js.meth
    method unshift_withString : Js.js_string Js.t -> unit Js.meth

  end

  let stream : stream Js.t = Bindings_utils.require_module "stream"

end

module Net = struct
  class type socket = object
    (* Actually they are both read and writeable *)
    inherit Stream.readable_stream

    method address : 'a Js.t Js.meth
    method bufferSize : int Js.readonly_prop
    method bytesRead : int Js.readonly_prop
    method bytesWritten : int Js.readonly_prop
    method connect :
      <port : int Js.prop;
       host: Js.js_string Js.t Js.prop;
       localAddress : Js.js_string Js.t Js.prop;
       localPort: int Js.prop;
       family : int Js.prop;
       hints : int Js.prop;
       lookup: 'a Js.callback Js.opt Js.t Js.prop> Js.t
      (* Many more connect methods needed *)
      (* method connect_with_listener : *)
    method connecting : bool Js.t Js.readonly_prop

    method destroy : unit Js.meth
    method destroy_with_exception : 'a Js.t -> unit Js.meth
    method destroyed : bool Js.t Js.readonly_prop
    method end_ : unit Js.meth
    method end_with_data : Buffer.buffer Js.t -> unit Js.meth
    method end_with_data_and_encoding :
      Buffer.buffer Js.t -> Js.js_string Js.t -> unit Js.meth
    method localAddress : Js.js_string Js.t Js.readonly_prop
    method localPort : int Js.readonly_prop
    (* method! pause : unit Js.meth *)
    method ref : unit Js.meth
    method remoteAddress : Js.js_string Js.t Js.readonly_prop
    method remoteFamily : Js.js_string Js.t Js.readonly_prop
    method remotePort : int Js.readonly_prop
    (* method resume : unit Js.meth *)
    (* method setEncoding : unit Js.meth *)
    method setEncoding_with_encoding : Js.js_string Js.t -> unit Js.meth
    method setKeepAlive : socket Js.t Js.meth
    method setKeepAlive_withDelay : bool Js.t-> int -> socket Js.t Js.meth
    method setNoDelay : socket Js.t Js.meth
    method setNoDeplay_withBool : bool Js.t -> socket Js.t Js.meth

    method write : Buffer.buffer Js.t -> unit Js.meth
    method write_withEncoding :
      Buffer.buffer Js.t -> Js.js_string Js.t -> unit Js.meth
    method write_withEncodingAndCallback :
      Buffer.buffer Js.t -> Js.js_string Js.t -> 'a Js.callback -> unit Js.meth

  end

  and server = object
    inherit Events.event_emitter
    method address : <port: int Js.readonly_prop;
                      family : Js.js_string Js.t Js.readonly_prop;
                      address : Js.js_string Js.t Js.readonly_prop > Js.t Js.meth
    method close : unit Js.meth
    method close_withCallback : (error_arg -> unit) Js.callback Js.meth
    method getConnections : (error_arg -> int -> unit) Js.callback Js.meth
    (* Later type it, a server or socket *)
    method listen : int -> (unit -> unit) Js.callback -> unit Js.meth

    method maxConnections : int Js.prop
    method listening : bool Js.t Js.readonly_prop
  end
  and net = object
    (* method connect : <port : int Js.prop> Js.t  *)
    method createServer : server Js.t Js.meth
    method createServer_withConnListener :
      (socket Js.t -> unit) Js.callback -> server Js.t Js.meth
    method createServer_withOptionsAndConnListener :
      <allowHalfOpen: bool Js.t Js.prop;
       pauseOnConnect : bool Js.t Js.prop> Js.t ->
      server Js.t Js.meth

  end

  (* let socket_with_options : 'a. *)
  (*   (<fd : 'a Js.opt Js.t Js.prop; *)
  (*     allowHalfOpen : bool Js.t Js.prop; *)
  (*     readable: bool Js.t Js.prop; *)
  (*     writable : bool Js.t Js.prop> Js.t -> socket Js.t) Js.constr = fun item -> *)
  (*   (Bindings_utils.require_module "net")##.Socket *)

  let socket : socket Js.t Js.constr =
    (Bindings_utils.require_module "net")##.Socket

  let net : net Js.t = Bindings_utils.require_module "net"

end

(* module Dns = struct *)
(*   class type dns = object *)
(*     method lookup : *)
(*       Js.js_string Js.t -> *)
(*       (Js.error -> Js.js_string Js.t -> int -> unit) Js.callback -> *)
(*       unit Js.meth *)

(*     method resolve4 : *)
(*       Js.js_string Js.t -> *)
(*       (Js.string_array Js.t -> unit) Js.callback -> *)
(*       unit Js.meth *)

(*   end *)

(*   let dns : dns Js.t = Bindings_utils.require_module "dns" *)

(* end *)


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

module Os = struct

  class type os = object
    method _EOL : Js.js_string Js.t Js.readonly_prop
    method arch : Js.js_string Js.t Js.meth
    (* method constants *)
    method cpus :
      <model : Js.js_string Js.t Js.readonly_prop;
       speed : int Js.readonly_prop;
       times : <user: int Js.readonly_prop;
                nice : int Js.readonly_prop;
                sys: int Js.readonly_prop;
                idle : int Js.readonly_prop;
                irq : int Js.readonly_prop> Js.t Js.readonly_prop>
        Js.t Js.js_array Js.t Js.meth
    method endianness : Js.js_string Js.t Js.meth
    method freemem : int Js.meth
    method homedir : Js.js_string Js.t Js.meth
    method hostname : Js.js_string Js.t Js.meth
    method loadavg : int Js.js_array Js.t Js.meth

  end

  let os : os Js.t = Bindings_utils.require_module "os"

end

module Http = struct

  class type agent = object
    (* method createConnection *)
    (* method destroy *)
    (* method freeSockets *)
    (* method getName *)
    (* method maxFreeSockets *)
    (* method maxSockets *)
    (* method requests *)
    (* method sockets *)
  end

  class type client_request = object
    inherit Stream.writable_stream

  end

  let agent : agent Js.t = (Bindings_utils.require_module "http")##.Agent

  let agent_with_options :
    (<keepAlive: bool Js.t Js.readonly_prop;
      keepAliveMsecs : int Js.readonly_prop;
      maxSockets : int Js.readonly_prop;
      maxFreeSockets : int Js.readonly_prop> Js.t -> agent Js.t) Js.constr =
    (Bindings_utils.require_module "http")##.Agent

end

class type process = object
  inherit Events.event_emitter
  method argv : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
  method abort : unit Js.meth
  method arch : Js.js_string Js.t Js.readonly_prop
  method chdir : Js.js_string Js.t -> unit Js.meth
  method config : 'a Js.t Js.readonly_prop
  method connected : bool Js.t Js.readonly_prop
  method cwd : Js.js_string Js.t Js.meth
  method disconnect : unit Js.meth
  method env : 'a Js.t Js.readonly_prop
  (* Many more missing *)
  method pid : int Js.readonly_prop
  method exit : int -> unit Js.meth
end

let process : process Js.t = Js.Unsafe.variable "process"
