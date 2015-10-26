open Nodejs_kit

class type incoming_message = object

  method httpVersion : js_str Js.readonly_prop

end

class type server_response = object

  method url : js_str Js.readonly_prop

  (** Be sure to be giving writeHead a JS Object and nothing else *)
  method writeHead : int -> Js.Unsafe.any Js.t -> unit Js.meth

  method writeHead_withMessage :
    int -> js_str -> 'a Js.t -> unit Js.meth

  method end_ : unit -> unit Js.meth

  method end_data : js_str -> unit Js.meth

end

class type client_request = object

  method flush_headers : unit -> unit Js.meth

  method write : js_str -> Js.json -> unit Js.meth

end

class type server = object

  inherit Events.event_emitter

  (** Give a port and callback, get the handle of the server back *)
  method listen : int -> (unit -> unit) Js.callback -> server Js.t Js.meth

  (** Just tell the server to listen on this port and callback,
      ignore the result *)
  method listen_ignore : int -> (unit -> unit) Js.callback -> unit Js.meth

  method close : (unit -> unit) Js.callback -> unit Js.meth

  (** A read only JavaScript object with meta data about current
      server *)
  method address :
    unit -> <address: js_str Js.readonly_prop;
             family : js_str Js.readonly_prop;
             port:    js_str Js.readonly_prop> Js.t Js.meth

end

class type http = object

  method methods : Js.js_string Js.js_array Js.t Js.readonly_prop

  method createServer_withapp : 'a Js.t -> server Js.t Js.meth

  method createServer :
    (incoming_message Js.t -> server_response Js.t -> unit) Js.callback ->
    server Js.t Js.meth

end

let require () : http Js.t = require "http"
