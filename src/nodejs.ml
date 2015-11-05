(** Raw call for doing require("some_module") *)
let require_module s =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "require")
    [|Js.Unsafe.inject (Js.string s)|]

let ( !@ ) f = Js.wrap_callback f

(** Get the field of a JavaScript Object *)
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
  |> Array.of_list |> Js.Unsafe.obj

(** Turn JavaScript string array into OCaml list of string *)
let to_string_list g =
  g |> Js.str_array |> Js.to_array |> Array.map Js.to_string |> Array.to_list

(* This will crap out at the moment because of int overflow on some
   field *)
(* let constants = require_module "constants" |> to_json *)

type memory_usage = { rss : int; heap_total : int; heap_used : int; }

class process = object

  val raw_js = Js.Unsafe.variable "process"

  method on_exit (f : (int -> unit)) : unit =
    m raw_js "on" [|i (Js.string "exit"); i !@f|]
  (* Type this stronger later *)
  method on_message (f : (Js.Unsafe.any -> Js.Unsafe.any -> unit)) : unit =
    m raw_js "on" [|i (Js.string "message"); i !@f|]

  method on_before_exit (f : (unit -> unit)) : unit =
    m raw_js "on" [|i (Js.string "beforeExit"); i !@f|]

  (* method on_uncaught_exception (f : ) *)
  (* method on_unhandled_rejection *)
  (* method rejections_handled *)

  method platform = raw_js <!> "platform" |> Js.to_string

  method version = raw_js <!> "version" |> Js.to_string

  method argv = raw_js <!> "argv" |> to_string_list

  method exec_path = raw_js <!> "execPath" |> Js.to_string

  method exec_argv = raw_js <!> "execArgv" |> to_string_list

  method abort : unit = m raw_js "abort" [||]

  method chdir s : unit = m raw_js "chdir" [|i (Js.string s)|]

  method cwd : string = m raw_js "cwd" [||] |> Js.to_string

  method exit_ (j : int) : unit = m raw_js "exit" [|i j|]

  method exit_code : int = raw_js <!> "exitCode"

  method get_gid : int = m raw_js "getgid" [||]

  method get_e_gid : int = m raw_js "getegid" [||]

  method set_gid (j : int) : unit = m raw_js "setgid" [|i j|]

  method set_e_gid (j : int) : unit = m raw_js "setegid" [|i j|]

  method get_uid : int = m raw_js "getuid" [||]

  method get_e_uid : int = m raw_js "geteuid" [||]

  method set_uid (j : int) : unit = m raw_js "setuid" [|i j|]

  method set_e_uid (j : int) : unit = m raw_js "seteuid" [|i j|]

  method get_groups : int list =
    m raw_js "getgroups" [||] |> Js.to_array |> Array.to_list

  (* method set_groups () *)

  method versions = raw_js <!> "versions" |> to_json

  method config = raw_js <!> "config" |> to_json

  method release = raw_js <!> "release" |> to_json

  method kill ?signal (j : int) : unit =
    match signal with
    | None -> m raw_js "kill" [|i j|]
    | Some (g : string) -> m raw_js "kill" [|i j|]

  method title = raw_js <!> "title" |> Js.to_string

  method set_title (s : string) = raw_js##.title := (Js.string s)

  method arch = raw_js <!> "platform" |> Js.to_string

  method memory_usage =
    let handle = m raw_js "memoryUsage" [||] in
    { rss = handle <!> "rss";
      heap_total = handle <!> "heapTotal";
      heap_used = handle <!> "heapUsed"; }

  (* method next_tick (f : (unit -> unit)) *)

  method umask mask : unit =
    match mask with
    | None -> m raw_js "umask" [||]
    | Some (j : int) -> m raw_js "umask" [|i j|]

  method uptime : int = m raw_js "uptime" [||]

  (* method hrtime : (int * int) = *)
  (* method send  *)

  method disconnect : unit = m raw_js "disconnect" [||]

end

module Error = struct

  class error  = object


  end

end


module Buffer = struct

  type encoding = Ascii | Utf_8 | Utf_16_le
                | Ucs_2 | Base_64 | Binary | Hex

  type buffer_init = Size of int
                   | Array of int
                   | String of (string * encoding)

  let string_of_encoding = function
    | Ascii -> "ascii"
    | Utf_8 -> "utf8"
    | Utf_16_le -> "utf16le"
    | Ucs_2 -> "ucs2"
    | Base_64 -> "base64"
    | Binary -> "binary"
    | Hex -> "hex"

  let raw_js = Js.Unsafe.variable "Buffer"

  class buffer raw_js = object(self)

    method raw_buffer : Js.Unsafe.any = raw_js

    (** The size of the buffer in bytes. Note that this is not
        necessarily the size of the contents. length refers to the amount
        of memory allocated for the buffer object. It does not change when
        the contents of the buffer are changed.*)
    method length : int = raw_js <!> "length"

    method write ?(offset=0) ?(encoding=Utf_8) ?length (s : string) : int =
      (match length with
       | None ->
         [|to_js_str s; i offset; i (self#length - offset); to_js_str (string_of_encoding encoding)|]
       | Some (j : int) ->
         [|to_js_str s; i offset; i j; to_js_str (string_of_encoding encoding)|])
      |> m raw_js "write"

    method to_string ?(encoding=Utf_8) ?(start=0) ?end_ () : string =
      (match end_ with
       | None ->
         [|to_js_str (string_of_encoding encoding); i start; i self#length|]
       | Some (j : int) ->
         [| to_js_str (string_of_encoding encoding); i start; i j|])
      |> m raw_js "toString" |> Js.to_string

    (** Returns a JSON-representation of the Buffer
        instance. JSON.stringify implicitly calls this function when
        stringifying a Buffer instance. *)
    method to_json =
      m raw_js "toJSON" [||] |> to_json

  end

  let new_buffer = function
    | Size i ->
      Js.Unsafe.js_expr (Printf.sprintf "new Buffer(%d)" i)
    | String (data, encoding) ->
      Js.Unsafe.js_expr (Printf.sprintf "new Buffer(%s, %s)" data (string_of_encoding encoding))
    (* Come back to this *)
    | _ -> assert false
  (* let copy_buffer (b : buffer) = Js.Unsafe. *)
  (* let new_buffer *)

  let is_encoding e : bool = m raw_js "isEncoding" [|to_js_str e|]

  let is_buffer (o : Js.Unsafe.any) : bool = m raw_js "isBuffer" [|i o|]

  let byte_length ?(encoding=Utf_8) s : int =
    m raw_js "byteLength" [|to_js_str s;
                            to_js_str (string_of_encoding encoding)|]


end


module Events = struct

  class event = object

    val raw_js = require_module "events"

    (* Not sure how to type the listener function *)
    (* method on_new_listener (f : (string -> Js.)) *)

  end
end

module Stream = struct

  let raw_js = require_module "stream"


  class readable raw_js = object
    (* method on_readable *)
    (* method on_data *)
    (* method on_end *)
    (* method on_read *)

    (* Make this better in its return value, harder to type *)
    method read = function
      | None -> (m raw_js "read" [||]) |> Js.to_string
      | Some (j : int) -> (m raw_js "read" [|i j|]) |> Js.to_string

  end

end

module Child_process = struct

  type child_proc = { pid : int;
                      env : string list; }
  (* env : (string * string) list; } *)

  class child_process = object

    val raw_js = require_module "child_process"

    method on_error (f: (Error.error -> unit)) : unit =
      m raw_js "on" [|i (Js.string "error"); i !@f|]

    method on_exit (f : (int -> string -> unit)) : unit =
      m raw_js "on" [|i (Js.string "exit"); i !@f|]

    method on_close (f : (int -> string -> unit)) : unit =
      m raw_js "on" [|i (Js.string "close"); i !@f|]

    method on_disconnect (f : (unit -> unit)) : unit =
      m raw_js "on" [|i (Js.string "disconnect"); i !@f|]

    method on_message (f : (Js.Unsafe.any -> Js.Unsafe.any-> unit)) : unit =
      m raw_js "on" [|i (Js.string "message"); i !@f|]

    method stdin = new Stream.readable (raw_js <!> "stdin")

    method stdout = new Stream.readable (raw_js <!> "stdout")

    method stderr = new Stream.readable (raw_js <!> "stderr")

    method pid : int = raw_js <!> "pid"

    method connected : bool = raw_js <!> "connected"

    method kill signal : unit =
      m raw_js "kill" [|i (Js.string signal)|]

    method spawn_sync (* ?opts *) cmd args : child_proc =
      let handle =
        [|i (Js.string cmd);
          i (List.map Js.string args |> Array.of_list |> Js.array)|]
        |> m raw_js "spawnSync"
      in
      let env =
        (handle <!> "envPairs")
        |> Js.to_array |> Array.map Js.to_string |> Array.to_list
      in
      {pid = handle <!> "pid"; env }

  end

end

module Crypto = struct

  type flag = Rsa | Dsa | Dh | Rand | Ecdh | Ecdsa | Ciphers | Digests | Store
            | Pkey_meth | Pkey_asn1_meth | All | None

  type engine = Id of int | Path of string

  class hmac raw_js = object

  end

  class crypto = object

    val raw_js = require_module "crypto"

    (* method set_engine ?(flag=All) e : unit = *)
    (*   match e with *)
    (*   | Id j -> m raw_js "setEngine" [|to_js_str j; to_js_str (string_of_flag flag)|] *)
    (* | Path s -> m raw_js "setEngine" [||] *)

    method get_ciphers = m raw_js "getCiphers" [||] |> to_string_list

    method get_hashes = m raw_js "getHashes" [||] |> to_string_list

    method get_curves = m raw_js "getCurves" [||] |> to_string_list

  end


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
      m raw_js "on" [| to_js_str "close"; i !@f|]

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
      m raw_js "on" [| to_js_str "close"; i !@f|]

    method on_finish (f : (unit -> unit)) : unit =
      m raw_js "on" [| to_js_str "finish"; i !@f|]

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
      m (require_module "http") "createServer" [|i !@handler|]

    (* method on_request *)
    (* method on_connection *)
    (* method on_close *)
    (* method on_check_continue *)
    (* method on_connect *)
    (* method on_upgrade *)
    (* method on_client_error *)

    method listen ~port:(port : int) (handler : (unit -> unit)) : server =
      m raw_js_server "listen" [|i port; i !@ handler|]

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
    let callback = !@callback in
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

module OS = struct

  type endian = Big_endian | Little_endian

  type times = { user: int; nice : int; sys : int; idle : int; irq : int; }
  type cpu = { model : string; speed : int; times : times; }

  type ip_family = Ip4 | Ip6

  type i_desc = { address : string;
                  netmask : string;
                  family : ip_family;
                  mac: string;
                  internal : bool; }

  type interface = { interface_name : string; interface_desc : i_desc list; }

  class os = object

    val raw_js = require_module "os"

    method tmpdir = m raw_js "tmpdir" [||] |> Js.to_string

    method homedir = m raw_js "homedir" [||] |> Js.to_string

    method endianness =
      match m raw_js "endianness" [||] |> Js.to_string with
      | "BE" -> Big_endian
      | "LE" -> Little_endian
      | _ -> assert false

    method hostname = m raw_js "hostname" [||] |> Js.to_string

    method os_type = m raw_js "type" [||] |> Js.to_string

    method platform = m raw_js "platform" [||] |> Js.to_string

    method arch = m raw_js "arch" [||] |> Js.to_string

    method release = m raw_js "release" [||] |> Js.to_string

    method uptime : int = m raw_js "uptime" [||]

    method loadavg : int list =
      m raw_js "loadavg" [||] |> Js.to_array |> Array.to_list

    method total_memory : int = m raw_js "totalmem" [||]

    method free_memory : int = m raw_js "freemem" [||]

    (* method network_interfaces  *)
    method cpus : cpu list =
      m raw_js "cpus" [||]
      |> Js.to_array |> Array.map begin fun o ->
        let t = o <!> "times" in
        {model = o <!> "model";
         speed = o <!> "speed";
         times = {user = t <!> "user" ;
                  nice = t <!> "nice";
                  sys = t <!> "sys";
                  idle = t <!> "idle";
                  irq = t <!> "irq"; }}
      end
      |> Array.to_list

    method eol = raw_js <!> "EOL" |> Js.to_string

  end

end

module URL = struct

  type url_parsed = { protocol : string option;
                      slashes: bool option;
                      auth: string option;
                      host: string option;
                      port: int option;
                      hostname: string option;
                      hash: string option;
                      search: string option;
                      query: string option;
                      pathname: string;
                      path: string;
                      href: string; }

  class url = object
    val raw_js = require_module "url"

    method parse (s : string) =
      let handle = m raw_js "parse" [|to_js_str s|] in
      let field name =
        if handle <!> name = Js.null then None else Some handle <!> name
      in
      {protocol = field "protocol";
       slashes = field "slashes";
       auth = field "auth";
       host = field "host";
       port = field "port";
       hostname = field "hostname";
       hash = field "hash";
       search = field "search";
       query = field "query";
       pathname = handle <!> "pathname";
       path = handle <!> "path";
       href = handle <!> "href";
      }

    (* method format = *)
    method resolve from to_ : string =
      m raw_js "resolve" [|to_js_str from; to_js_str to_|] |> Js.to_string

  end

end
