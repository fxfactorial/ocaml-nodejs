let require_module s =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "require")
    [|Js.Unsafe.inject (Js.string s)|]

(** Same as console.log *)
let log obj = Firebug.console##log obj

(** The current file name *)
let __filename () =
  (Js.Unsafe.eval_string "__filename" : Js.js_string Js.t) |> Js.to_string

(** The current directory name *)
let __dirname () =
  (Js.Unsafe.eval_string "__dirname" : Js.js_string Js.t) |> Js.to_string

module type Http = sig

  type methods = Checkout | Connect | Copy | Delete | Get | Head | Lock |
                 M_search | Merge | Mkactivity | Mkcalendar | Mkcol |
                 Move | Notify | Options | Patch | Post | Propfind |
                 Proppath | Purge | Put | Report | Search | Subscribe |
                 Trace | Unlock | Unsubscribe

  class type incoming_message = object  end

  class type server_response = object end

  class type server = object
    method listen : port:int -> callback:(unit -> unit) -> unit
  end

  val create_server : (incoming_message -> server_response -> unit) -> server

end

module Http = struct

  type methods = Checkout | Connect | Copy | Delete | Get | Head | Lock |
                 M_search | Merge | Mkactivity | Mkcalendar | Mkcol |
                 Move | Notify | Options | Patch | Post | Propfind |
                 Proppath | Purge | Put | Report | Search | Subscribe |
                 Trace | Unlock | Unsubscribe

  module Raw_js = struct

    class type http = object end
    class type server = object end

  end

  class incoming_message = object end
  class server_response = object end

  class server handler = object(self)

    val raw_js_server : Raw_js.server Js.t =
      let h : Raw_js.http Js.t = require_module "http" in
      Js.Unsafe.meth_call h "createServer" [|Js.Unsafe.inject handler|]

    method listen (port:int) (handler : (unit -> unit)) : unit =
      Js.Unsafe.meth_call raw_js_server "listen" [|Js.Unsafe.inject port;
                                                   Js.Unsafe.inject handler|]

  end

  let create_server handler =
    new server handler

end

type modules = Http

let require module_ =
  match module_ with
  | Http -> (module Http : Http)
