let require_module s =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "require")
    [|Js.Unsafe.inject (Js.string s)|]

(** Use for JavaScript Object literals *)
let ( !! ) i = Js.Unsafe.inject i

let ( <!> ) obj field = Js.Unsafe.get obj field

(** Same as console.log *)
let log obj = Firebug.console##log obj

(** The current file name *)
let __filename () =
  (Js.Unsafe.eval_string "__filename" : Js.js_string Js.t) |> Js.to_string

(** The current directory name *)
let __dirname () =
  (Js.Unsafe.eval_string "__dirname" : Js.js_string Js.t) |> Js.to_string

(** Call method of a JavaScript object *)
let m = Js.Unsafe.meth_call

(** Inject something as a JS object, be sure its Js.t already,
    functions seemingly exempt *)
let i = Js.Unsafe.inject

(** Turn a JavaScript Object into a string *)
let stringify o =
  m (Js.Unsafe.variable "JSON") "stringify" [|i o|] |> Js.to_string

(** Turn an OCaml string into a JavaScript string *)
let to_js_str s = Js.string s |> Js.Unsafe.inject

(** Turn a JavaScript Object into a Yojson object *)
let to_json obj =
  stringify obj |> Yojson.Basic.from_string

(** Create a JavaScript Object out of an alist *)
let obj_of_alist a_l =
  List.map (fun (key, value) -> (key, Js.Unsafe.inject value)) a_l
  |> Array.of_list
  |> Js.Unsafe.obj

class process = object

  val raw_process = Js.Unsafe.variable "process"

  (* method on_exit *)
  (* method on_message *)
  (* method on_before_exit *)
  (* method on_uncaught_exception *)
  (* method on_unhandled_rejection *)
  (* method rejections_handled *)

  method platform =
    raw_process <!> "platform" |> Js.to_string

  method version =
    raw_process <!> "version" |> Js.to_string

end

module Error = struct
  class error = object end
end


module Buffer = struct
  class buffer = object end
end


module Events = struct
  class event = object end
end

module Http = struct

  type methods = Checkout | Connect | Copy | Delete | Get | Head | Lock |
                 M_search | Merge | Mkactivity | Mkcalendar | Mkcol |
                 Move | Notify | Options | Patch | Post | Propfind |
                 Proppath | Purge | Put | Report | Search | Subscribe |
                 Trace | Unlock | Unsubscribe

  type chunk = String of string | Buffer of Buffer.buffer

  let status_codes =
    (require_module "http") <!> "STATUS_CODES" |> to_json
    |> Yojson.Basic.Util.to_assoc
    |> List.map begin fun (code, message) ->
      (int_of_string code, Yojson.Basic.Util.to_string message)
    end

  class incoming_message raw_js = object

    method http_version =
      raw_js <!> "httpVersion" |> Js.to_string

    method on_close (f : (unit -> unit)) : unit =
      m raw_js "on" [| to_js_str "close"; Js.Unsafe.inject f|]

    method headers =
      raw_js <!> "headers" |> to_json
      |> Yojson.Basic.Util.to_assoc
      |> List.map (fun (a, b) -> (a, Yojson.Basic.Util.to_string b))

    method raw_headers =
      (raw_js <!> "rawHeaders" : Js.js_string Js.t Js.js_array Js.t)
      |> Js.to_array
      |> Array.map Js.to_string
      |> Array.to_list

  end

  class server_response raw_js = object

    method on_close (f : (unit -> unit)) : unit =
      m raw_js "on" [| to_js_str "close"; Js.Unsafe.inject f|]

    method on_finish (f : (unit -> unit)) : unit =
      m raw_js "on" [| to_js_str "finish"; Js.Unsafe.inject f|]

    method write_continue : unit =
      m raw_js "writeContinue" [||]

    method write_head
        ?status_message
        ~status_code:(status_code : int)
        (headers : (string * string) list) : unit =
      match status_message with
      | None -> m raw_js "writeHead" [|i status_code; i (obj_of_alist headers)|]
      | Some msg ->
        m raw_js "writeHead" [|i status_code;
                               i (Js.string msg);
                               i (obj_of_alist headers)|]

    (* method set_timeout *)
    (* method status_code *)
    (* method status_message *)
    (* method set_header *)
    (* method headers_sent *)
    (* method send_date *)
    (* method get_header *)
    (* method remove_header *)
    method write
        ?(callback : (unit -> unit) option)
        ?(encoding: string option)
        chunk : unit =
      match chunk with
      | String s ->
        m raw_js "write" [|to_js_str s|]
      | _ -> assert false

    (* method add_trailers *)

    method end_
      ?(data : chunk option)
      ?(encoding: string option)
      ?(callback : (unit -> unit) option)
      ()
      : unit =
      match data with
      | Some (String s) ->
        m raw_js "end" [|to_js_str s|]
      | _ -> assert false

    (* method finished *)

  end

  class server handler = object(self)

    val raw_js_server =
      m (require_module "http") "createServer" [|i (Js.wrap_callback handler)|]

    (* method on_request *)
    (* method on_connection *)
    (* method on_close *)
    (* method on_check_continue *)
    (* method on_connect *)
    (* method on_upgrade *)
    (* method on_client_error *)

    method listen ~port:(port : int) (handler : (unit -> unit)) : server =
      m raw_js_server "listen" [|i port; i (Js.wrap_callback handler)|]

  end

  let create_server handler =
    let wrapped_handler = fun incoming_msg response ->
      handler (new incoming_message incoming_msg) (new server_response response)
    in
    new server wrapped_handler

end

module Fs = struct

  type flag = Read | Write | Read_write | Append

  let string_of_flag = function
    | Read -> "r"
    | Write -> "w"
    | _ -> "r"

  type options = { encoding : string option; flag : flag option }

  let read_file
      ?options
      ~path:(path : string)
      (callback : (Error.error -> string -> unit)) =
    let fs = require_module "fs" in
    let path = Js.string path in
    let callback = Js.wrap_callback callback in
    (* Things given to inject need to already be Js.t objects *)
    match options with
    | None ->
      m fs "readFile" [|i path; i callback|]
    | Some opts -> match opts with
      | {encoding = None ; flag = None } ->
        m fs "readFile" [|i path; i callback|]
      | {encoding = Some e; flag = None } ->
        m fs "readFile" [|i path; i e; i callback|]
      | _ -> ()

end
