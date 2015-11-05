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

  class error = object end

end


module Buffer = struct

  class buffer = object end

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
