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

let m = Js.Unsafe.meth_call
let i = Js.Unsafe.inject

module type Error = sig
  class type error = object end
end

module Error = struct
  class error = object
  end
end

module type Http = sig

  type methods = Checkout | Connect | Copy | Delete | Get | Head | Lock |
                 M_search | Merge | Mkactivity | Mkcalendar | Mkcol |
                 Move | Notify | Options | Patch | Post | Propfind |
                 Proppath | Purge | Put | Report | Search | Subscribe |
                 Trace | Unlock | Unsubscribe

  class type incoming_message = object  end

  class type server_response = object end

  class type server = object
    method listen : int -> (unit -> unit) -> unit
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
  class server_response = object

  end

  class server handler = object(self)

    val raw_js_server : Raw_js.server Js.t =
      let h : Raw_js.http Js.t = require_module "http" in
      m h "createServer" [|i handler|]

    method listen (port:int) (handler : (unit -> unit)) : unit =
      m raw_js_server "listen" [|i port; i handler|]

  end

  let create_server handler =
    new server handler

end

module type Fs = sig
  type flag = Read | Write | Read_write | Append
  type options = { encoding : string option; flag : flag option }

  val read_file :
    string -> options option -> (Error.error -> string -> unit) -> unit
end

module Fs = struct

  module Raw_js = struct
    class type fs = object end
  end

  type flag = Read | Write | Read_write | Append

  let string_of_flag = function
    | Read -> "r"
    | Write -> "w"
    | _ -> "r"

  type options = { encoding : string option; flag : flag option }

  let read_file
      (path : string)
      opts
      (callback : (Error.error -> string -> unit)) =
    let fs : Raw_js.fs Js.t = require_module "fs" in
    let path = Js.string path in
    (* Things given to inject need to already be Js.t objects *)
    match opts with
    | None ->
      m fs "readFile" [|i path; i callback|]
    | Some opts -> match opts with
      | {encoding = None ; flag = None } ->
        m fs "readFile" [|i path; i callback|]
      | {encoding = Some e; flag = None } ->
        m fs "readFile" [|i path; i e; i callback|]
      | _ -> ()
end

type 'a modules = Http : (module Http) modules
                | Fs : (module Fs) modules
                | Error : (module Error) modules

let require : type a . a modules -> a = function
  | Http -> (module Http : Http)
  | Fs -> (module Fs : Fs)
  | Error -> (module Error : Error)
