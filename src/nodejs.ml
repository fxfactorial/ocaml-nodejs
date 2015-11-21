(** Raw call for doing require("some_module") *)
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
let stringify o = Js._JSON##stringify o |> Js.to_string

(** Turn an OCaml string into a JavaScript string *)
let to_js_str s = Js.string s |> Js.Unsafe.inject

(** Turn a JavaScript Object into a Yojson object *)
let to_json obj = stringify obj |> Yojson.Basic.from_string

(** Turn Yojson object into JavaScript Object *)
let of_json j = Js._JSON##parse (j |> Yojson.Basic.to_string |> Js.string)

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

(* This will crap out at the moment because of int overflow on some
   field *)
(* let constants = require_module "constants" |> to_json *)

let lookup_mime s = Magic_mime.lookup s

let module_handle = Js.Unsafe.js_expr "module"

(** Provide a package that can be loaded with a
    require("package_name"), everything in the resultant object is the
    "public" interface of the library, so to speak. *)
let exports exps =
  let obj = obj_of_alist exps in
  module_handle##.exports := obj

type memory_usage = { rss : int; heap_total : int; heap_used : int; }

type ip_family = Ip4 | Ip6

type encoding = Ascii | Utf_8 | Utf_16_le
              | Ucs_2 | Base_64 | Binary | Hex

type platform = Darwin | Freebsd | Linux | Sunos | Win32

let int_of_ip_family = function
  | Ip4 -> 4 | Ip6 -> 6

let ip_of_string = function
  | "IPv4" -> Ip4 | "IPv6" -> Ip6 | _ -> assert false

let string_of_ip = function
  | Ip4 -> "IPv4" | Ip6 -> "Ipv6"

let ip_of_int = function
  | 4 -> Ip4 | 6 -> Ip6 | _ -> assert false

let string_of_platform = function
  | Darwin -> "darwin"
  | Freebsd -> "freebsd"
  | Linux -> "linux"
  | Sunos -> "sunos"
  | Win32 -> "win32"

let string_of_encoding = function
  | Ascii -> "ascii"
  | Utf_8 -> "utf8"
  | Utf_16_le -> "utf16le"
  | Ucs_2 -> "ucs2"
  | Base_64 -> "base64"
  | Binary -> "binary"
  | Hex -> "hex"

type signal = SIG_HUP | SIG_INT | SIG_QUIT | SIG_ILL
            | SIG_TRAP | SIG_ABRT | SIG_EMT | SIG_FPE
            | SIG_KILL | SIG_BUS | SIG_SEGV | SIG_SYS
            | SIG_PIPE | SIG_ALRM | SIG_TERM | SIG_URG
            | SIG_STOP | SIG_TSTP | SIG_CONT | SIG_CHLD
            | SIG_TTIN | SIG_TTOU | SIG_IO | SIG_XCPU
            | SIG_XFSZ | SIG_VTALRM | SIG_PROF | SIG_WINCH
            | SIG_INFO | SIG_USR1 | SIG_USR2

let string_of_signal = function
  | SIG_HUP  -> "SIGHUP"   | SIG_INT    -> "SIGINT"
  | SIG_QUIT -> "SIGQUIT"  | SIG_ILL    -> "SIGILL"
  | SIG_TRAP -> "SIGTRAP"  | SIG_ABRT   -> "SIGABRT"
  | SIG_EMT  -> "SIGEMT"   | SIG_FPE    -> "SIGFPE"
  | SIG_KILL -> "SIGKILL"  | SIG_BUS    -> "SIGBUS"
  | SIG_SEGV -> "SIGSEGV"  | SIG_SYS    -> "SIGSYS"
  | SIG_PIPE -> "SIGPIPE"  | SIG_ALRM   -> "SIGALRM"
  | SIG_TERM -> "SIGTERM"  | SIG_URG    -> "SIGURG"
  | SIG_STOP -> "SIGSTOP"  | SIG_TSTP   -> "SIGTSTP"
  | SIG_CONT -> "SIGCONT"  | SIG_CHLD   -> "SIGCHLD"
  | SIG_TTIN -> "SIGTTIN"  | SIG_TTOU   -> "SIGTTOU"
  | SIG_IO   -> "SIGIO"    | SIG_XCPU   -> "SIGXCPU"
  | SIG_XFSZ -> "SIGXFSZ"  | SIG_VTALRM -> "SIGVTALRM"
  | SIG_PROF -> "SIGPROF"  | SIG_WINCH  -> "SIGWINCH"
  | SIG_INFO -> "SIG_INFO" | SIG_USR1   -> "SIGUSR1"
  | SIG_USR2 -> "SIGUSR2"

let signal_of_string = function
  | "SIGHUP"   -> SIG_HUP  | "SIGINT"    -> SIG_INT
  | "SIGQUIT"  -> SIG_QUIT | "SIGILL"    -> SIG_ILL
  | "SIGTRAP"  -> SIG_TRAP | "SIGABRT"   -> SIG_ABRT
  | "SIGEMT"   -> SIG_EMT  | "SIGFPE"    -> SIG_FPE
  | "SIGKILL"  -> SIG_KILL | "SIGBUS"    ->  SIG_BUS
  | "SIGSEGV"  -> SIG_SEGV | "SIGSYS"    -> SIG_SYS
  | "SIGPIPE"  -> SIG_PIPE | "SIGALRM"   -> SIG_ALRM
  | "SIGTERM"  -> SIG_TERM | "SIGURG"    -> SIG_URG
  | "SIGSTOP"  -> SIG_STOP | "SIGTSTP"   ->  SIG_TSTP
  | "SIGCONT"  -> SIG_CONT | "SIGCHLD"   ->  SIG_CHLD
  | "SIGTTIN"  -> SIG_TTIN | "SIGTTOU"   -> SIG_TTOU
  | "SIGIO"    -> SIG_IO   | "SIGXCPU"   -> SIG_XCPU
  | "SIGXFSZ"  -> SIG_XFSZ | "SIGVTALRM" -> SIG_VTALRM
  | "SIGPROF"  -> SIG_PROF | "SIGWINCH"  -> SIG_WINCH
  | "SIG_INFO" -> SIG_INFO | "SIGUSR1"   -> SIG_USR1
  | "SIGUSR2"  -> SIG_USR2 | _ -> assert false

module Error = struct

  class error raw_js = object

    method code : int = raw_js <!> "code"

    method message = raw_js <!> "message" |> Js.to_string

  end

end

module Buffer = struct

  let raw_js = Js.Unsafe.variable "Buffer"

  class type raw_buf = object end

  let new_buffer_array : (int Js.js_array Js.t -> raw_buf Js.t) Js.constr =
    Js.Unsafe.variable "Buffer"

  let new_buffer_raw : (Js.Unsafe.any -> raw_buf Js.t) Js.constr =
    Js.Unsafe.variable "Buffer"

  type buffer_init_t = [`Array of int array
                       | `Existing of Js.Unsafe.any]

  class buffer (input : buffer_init_t) = object(self)

    val raw_js = match input with
      | `Array a -> i (new%js new_buffer_array (Js.array a))
      | `Existing e -> e

    method unsafe_raw : Js.Unsafe.any = i raw_js

    (** The size of the buffer in bytes. Note that this is not
        necessarily the size of the contents. length refers to the amount
        of memory allocated for the buffer object. It does not change when
        the contents of the buffer are changed.*)
    method length : int = raw_js <!> "length"

    method write ?(offset=0) ?(encoding=Utf_8) ?length (s : string) : int =
      (match length with
       | None ->
         [|to_js_str s; i offset; i (self#length - offset);
           to_js_str (string_of_encoding encoding)|]
       | Some (j : int) ->
         [|to_js_str s; i offset; i j;
           to_js_str (string_of_encoding encoding)|])
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
    (* TODO Something buggy with this *)
    (* method to_json = *)
    (*   m raw_js "toJSON" [||] |> to_json *)

  end

  (* let new_buffer = function *)
  (*   | Size i -> *)
  (*     Js.Unsafe.js_expr (Printf.sprintf "new Buffer(%d)" i) *)
  (*   | String (data, encoding) -> *)
  (*     Js.Unsafe.js_expr *)
  (*       (Printf.sprintf *)
  (*          "new Buffer(%s, %s)" data (string_of_encoding encoding)) *)
  (*   (\* Come back to this *\) *)
  (*   | _ -> assert false *)
  (* let copy_buffer (b : buffer) = Js.Unsafe. *)
  (* let new_buffer *)

  let is_encoding e : bool = m raw_js "isEncoding" [|to_js_str e|]

  let is_buffer (o : Js.Unsafe.any) : bool = m raw_js "isBuffer" [|i o|]

  let byte_length ?(encoding=Utf_8) s : int =
    m raw_js "byteLength" [|to_js_str s;
                            to_js_str (string_of_encoding encoding)|]

end

type str_or_buff = String of string | Buffer of Buffer.buffer

module Events = struct

  class event = object

    val events_module = require_module "events"

    method add_listener s (f : Js.Unsafe.any -> unit) : unit =
      m events_module "addListener" [|to_js_str s; i !@f|]

    method on s (f : Js.Unsafe.any -> unit) : unit =
      m events_module "addListener" [|to_js_str s; i !@ f|]

    method once s (f : Js.Unsafe.any -> unit) : unit =
      m events_module "once" [|to_js_str s; i !@ f|]

    (** Removes a listener from the listener array for the specified
        event. Caution: changes array indices in the listener array behind
        the listener.*)
    method remove_listener s (f : Js.Unsafe.any -> unit) : unit =
      m events_module "removeListener" [|to_js_str s; i !@f|]

    method remove_all_listeners s : unit =
      match s with
      | None -> m events_module "removeListener" [||]
      | Some event -> m events_module "removeListener" [|to_js_str event|]

    method set_max_listeners (n : int) : unit =
      m events_module "setMaxListeners" [|i n;|]

    method get_max_listeners : int = m events_module "getMaxListeners" [||]

    method get_default_max_listeners : int =
      events_module <!> "defaultMaxListeners"

    method set_default_max_listeners (n : int) =
      Js.Unsafe.set events_module "defaultMaxListeners" n

  end

end


module Stream = struct

  let raw_js = require_module "stream"

  class duplex r = object

    inherit Events.event

    (** DO NOT TOUCH THIS *)
    method unsafe_raw = r

    method pipe ?(end_=true) (dest : duplex) =
      (if end_ then m r "pipe" [|dest#unsafe_raw|]
       else begin
         let obj = object %js end in
         Js.Unsafe.set obj "end" (Js.bool false);
         m r "pipe" [|dest#unsafe_raw;i (object%js end) |]
       end)
      |> new duplex

  end
  and readable raw = object

    inherit duplex raw

    method on_readable (f : (unit -> unit)) : unit =
      m raw "on" [|to_js_str "readable"; i !@f|]

    method on_data (f : (str_or_buff -> unit)) : unit =
      let wrapped = fun raw_buffer ->
        if (Js.typeof raw_buffer |> Js.to_string) <> "string"
        then f (Buffer (new Buffer.buffer (`Existing (i raw_buffer))))
        else f (String (raw_buffer |> Js.to_string))
      in
      m raw "on" [|to_js_str "data"; i !@wrapped|]

    method on_end (f : (unit -> unit)) : unit =
      m raw "on" [|to_js_str "end"; i !@ f|]

    method on_close (f : (unit -> unit)) : unit =
      m raw "on" [|to_js_str "close"; i !@f|]

    method on_error (f : Error.error -> unit) : unit =
      let g = fun raw_error -> f (new Error.error raw_error) in
      m raw "on" [|to_js_str "error"; i !@g|]

    method is_paused = m raw "isPaused" [||] |> Js.to_bool

    method pause : unit = m raw "pause" [||]

    (* This is incorrect *)
    method read = function
      | None -> (m raw_js "read" [||]) |> Js.to_string
      | Some (j : int) -> (m raw "read" [|i j|]) |> Js.to_string

    method set_encoding e : unit =
      m raw "setEncoding" [|string_of_encoding e |> to_js_str|]

    method resume : unit = m raw "resume" [||]

  end

  and writable raw_js = object

    (* inherit Events.event *)
    inherit duplex raw_js

    (* method unsafe_raw = raw_js *)

    method on_drain (f : unit -> unit) : unit =
      m raw_js "on" [|to_js_str "drain"; i !@f|]

    method on_finish (f : unit -> unit) : unit =
      m raw_js "on" [|to_js_str "finish"; i !@f|]

    method on_pipe (f : readable -> unit) : unit =
      let g = fun r -> f (new readable r) in
      m raw_js "on" [|to_js_str "pipe"; i !@g|]

    method on_unpipe (f : readable -> unit) : unit =
      let g = fun r -> f (new readable r) in
      m raw_js "on" [|to_js_str "unpipe"; i !@g|]

    (* method on_error *)
    method cork : unit = m raw_js "cork" [||]

    method uncork : unit = m raw_js "uncork" [||]

    method set_default_encoding e : unit =
      m raw_js "setDefaultEncoding" [|string_of_encoding e |> to_js_str|]

  end

end
class process = object

  inherit Events.event

  val raw_js = Js.Unsafe.variable "process"

  method on_before_exit (f : (unit -> unit)) : unit =
    m raw_js "on" [|i (Js.string "beforeExit"); i !@f|]

  method on_exit (f : (int -> unit)) : unit =
    m raw_js "on" [|i (Js.string "exit"); i !@f|]
  (* Type this stronger later *)
  method on_message (f : (Js.Unsafe.any -> Js.Unsafe.any -> unit)) : unit =
    m raw_js "on" [|i (Js.string "message"); i !@f|]


  (* method on_uncaught_exception (f : ) *)
  (* method on_unhandled_rejection *)
  (* method rejections_handled *)

  method get_env (s : string) =
    try Some ((raw_js <!> "env") <!> (String.uppercase s) |> Js.to_string)
    with _ -> None

  method platform = match raw_js <!> "platform" |> Js.to_string with
    | "darwin" -> Darwin
    | "freebsd" -> Freebsd
    | "linux" -> Linux
    | "sunos" -> Sunos
    | "win32" -> Win32
    | _ -> assert false

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
    | Some (g : signal) ->
      m raw_js "kill" [|i j; to_js_str (string_of_signal g)|]

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

  method stderr = raw_js <!> "stderr" |> new Stream.writable

  method stdin = raw_js <!> "stdin" |> new Stream.readable

  method stdout = raw_js <!> "stdout" |> new Stream.writable

end

module Crypto = struct

  type flag = Rsa | Dsa | Dh | Rand | Ecdh | Ecdsa | Ciphers | Digests | Store
            | Pkey_meth | Pkey_asn1_meth | All | None

  type engine = Id of int | Path of string

  (* class hmac raw_js = object *)

  (* end *)

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

(** This module contains functions that belong to two different categories:

    1) Functions that use the underlying operating system facilities
    to perform name resolution, and that do not necessarily do any
    network communication. This category contains only one function:
    lookup.

    Developers looking to perform name resolution in the same way that
    other applications on the same operating system behave should use
    lookup

    2) Functions that connect to an actual DNS server to perform name
    resolution, and that always use the network to perform DNS
    queries. This category contains all functions in the dns module
    but lookup. These functions do not use the same set of
    configuration files than what lookup uses. For instance, they do
    not use the configuration from /etc/hosts. These functions should
    be used by developers who do not want to use the underlying
    operating system's facilities for name resolution, and instead
    want to always perform DNS queries. *)
module Dns = struct

  type dns_hint =
    (** Returned address types are determined by the types of
        addresses supported by the current system. For example, IPv4
        addresses are only returned if the current system has at least one
        IPv4 address configured. Loopback addresses are not considered. *)
      Addr_config
    (** If the IPv6 family was specified, but no IPv6 addresses
        were found, then return IPv4 mapped IPv6 addresses. Note that
        it is not supported on some operating systems (e.g FreeBSD
        10.1).*)
    | V4_mapped

  type lookup_opts =
    { (** If None, then both IP v4 and v6 addresses are accepted. *)
      ip_family : ip_family option;
      (** List of valid hints, will be logically ORed *)
      dns_hints  : dns_hint list; }

  type addresses = string list

  let hint_of_int = function
    | 1024 -> Addr_config | 2048 -> V4_mapped | _ -> assert false

  let int_of_hint = function Addr_config -> 1024 | V4_mapped -> 2048

  let raw_js = require_module "dns"

  (** Callback gets an empty list of addresses if none found for this
      host *)
  let lookup ~opts ~host
      (f : (Error.error -> addresses -> ip_family -> unit)) : unit =

    let wrapped = fun e addr fam ->
      f (new Error.error e) ([addr |> Js.to_string]) (ip_of_int fam)
    in

    match opts with {ip_family = f_opt; dns_hints = l } -> match (f_opt, l) with
      | (None, []) ->
        m raw_js "lookup" [|to_js_str host; i !@ wrapped|]
      | (None, l) ->
        let with_hints =
          !!(object%js
            val hints = List.fold_left ( lor ) 0 (List.map int_of_hint l)
          end)
        in
        m raw_js "lookup" [|to_js_str host; with_hints; i !@ wrapped|]
      | (Some ip, []) ->
        let with_ip = !!(object%js val family = int_of_ip_family ip end) in
        m raw_js "lookup" [|to_js_str host; with_ip; i !@ wrapped|]

      | (Some ip, l) ->
        let with_both =
          !!(object%js
            val family = int_of_ip_family ip
            val hints = List.fold_left ( lor ) 0 (List.map int_of_hint l)
          end)
        in
        m raw_js "lookup" [|to_js_str host; with_both; i !@ wrapped|]

  let lookup_service ~addr ~port
      (f : (Error.error -> string -> string -> unit)) : unit =
    m raw_js "lookupService" [|to_js_str addr; i (port : int); i !@ f|]

end

module Net = struct

  let raw_net_module = require_module "net"

  type server_opts = { allow_half_open : bool; pause_on_connect : bool; }

  type address = { port : int; ip_family: ip_family; address : string; }

  class server raw_js = object

    inherit Events.event

    method on_close (f : unit -> unit) : unit =
      m raw_js "on" [|to_js_str "close"; i !@f|]

    method on_connection (f : socket -> unit) : unit =
      let g = fun r -> f (new socket r) in
      m raw_js "on" [|to_js_str "connection"; i !@g|]

    (* method on_error  *)

    method on_listening (f : unit -> unit) : unit =
      m raw_js "on" [|to_js_str "listening"; i !@f|]

    method address =
      let h = m raw_js "address" [||] in
      { port = h <!> "port";
        ip_family = h <!> "family" |> Js.to_string |> ip_of_string;
        address = h <!> "address" |> Js.to_string; }

    method listen ~port:(p : int) (f : (unit -> unit)) : unit =
      m raw_js "listen" [|i p; i !@f|]

  end

  and socket raw_js = object

    inherit Events.event
    inherit Stream.duplex raw_js
    inherit Stream.readable raw_js

    method on_connect (f : unit -> unit) : unit =
      m raw_js "on" [|to_js_str "connect"; i !@f|]

    (* method on_lookup () *)
    method on_timeout (f : unit -> unit) : unit =
      m raw_js "on" [|to_js_str "timeout"; i !@f|]

    method write s : unit = m raw_js "write" [|to_js_str s|]

    method address =
      let h = m raw_js "address" [||] in
      { port = h <!> "port";
        ip_family = h <!> "family" |> Js.to_string |> ip_of_string;
        address = h <!> "address" |> Js.to_string; }

    (** Net.socket has the property that socket#write always
        works. This is to help users get up and running quickly. The
        computer cannot always keep up with the amount of data that is
        written to a socket - the network connection simply might be too
        slow. Node.js will internally queue up the data written to a
        socket and send it out over the wire when it is
        possible. (Internally it is polling on the socket's file
        descriptor for being writable).

        The consequence of this internal buffering is that memory may
        grow. This property shows the number of characters currently
        buffered to be written. (Number of characters is approximately
        equal to the number of bytes to be written, but the buffer may
        contain strings, and the strings are lazily encoded, so the
        exact number of bytes is not known.)

        Users who experience large or growing bufferSize should
        attempt to "throttle" the data flows in their program with
        #pause and #resume. *)
    method buffer_size : int = raw_js <!> "bufferSize"

    method bytes_read : int = raw_js <!> "bytesRead"

    method bytes_written : int = raw_js <!> "bytesWritten"

    (* method connect  *)

    method destory : unit = m raw_js "destroy" [||]

    method remote_address = raw_js <!> "remoteAddress" |> Js.to_string

    method remote_family =
      raw_js <!> "remoteFamily" |> Js.to_string |> ip_of_string

    method remote_port : int = raw_js <!> "port"

  end

  type socket_opts = { port : int;
                       host : string option;
                       local_address : string option;
                       local_port : int option;
                       ip_family : ip_family option;
                       (* This lookup signature is incorrect *)
                       lookup : (unit -> unit) option;
                       path : string option; }

  let obj_of_socket_opts
      { port; host; local_address; local_port; ip_family; lookup; path; } =
    let h = !!(object%js
        val port = port
        val host =
          (match host with None -> "localhost" | Some s -> s) |> Js.string
        val family =
          match ip_family with None -> 4 | Some ip -> int_of_ip_family ip
      end)
    in
    (match local_address with
     | None -> ()
     | Some addr -> Js.Unsafe.set h "localAddress" (Js.string addr));
    (match local_port with
     | None -> ()
     | Some j -> Js.Unsafe.set h "localPort" j);
    (match lookup with
     | None -> ()
     | Some f -> Js.Unsafe.set h "lookup" !@f);
    (match path with
     | None -> ()
     | Some s -> Js.Unsafe.set h "path" (Js.string s));
    h

  let connect
      ?(conn_listener : (unit -> unit) option)
      (s_opts : [`Path of string | `Opts of socket_opts]) =
    (match s_opts, conn_listener with
     | `Path p, None ->
       m raw_net_module "connect" [|to_js_str p|]
     | `Opts s_opts, None ->
       m raw_net_module "connect" [|obj_of_socket_opts s_opts|]
     | `Path p, Some f ->
       m raw_net_module "connect" [|to_js_str p; i !@f|]
     | `Opts s_opts, Some f ->
       m raw_net_module "connect" [|obj_of_socket_opts s_opts; i !@f|])
    |> new socket

  let create_server ?opts ?(conn_listener : (socket -> unit) option) () =
    let obj_of_s_opt {allow_half_open; pause_on_connect; } =
      !!(object%js
        val allowHalfOpen = allow_half_open
        val pauseOnConnect = pause_on_connect
      end)
    in
    (match (opts, conn_listener) with
     | None, None -> m raw_net_module "createServer" [||]
     | Some o, None ->
       m raw_net_module "createServer" [|obj_of_s_opt o|]
     | None, Some cb ->
       let g = fun raw_s -> cb (new socket raw_s) in
       m raw_net_module "createServer" [|i !@g|]
     | Some o, Some cb ->
       let g = fun raw_s -> cb (new socket raw_s) in
       m raw_net_module "createServer" [|obj_of_s_opt o; i !@g|])
    |> new server

end

module Child_process = struct

  type child_proc = { pid : int;
                      env : string list; }
  (* env : (string * string) list; } *)

  type exec_opts = { cwd : string;
                     env : (string * string) list;
                     encoding : encoding;
                     shell : string;
                     timeout: int;
                     max_buffer_size : int;
                     kill_signal: signal;
                     uid : int;
                     gid : int; }

  let exec_async
      ?opts
      ?f:(f : (Error.error -> Buffer.buffer -> Buffer.buffer) option)
      (* Shouldn't be unit *)
      cmd : unit =
    let handle = require_module "child_process" in
    match (opts, f) with
    | (None, None) -> m handle "exec" [|to_js_str cmd|] |> ignore
    (* TODO Implement this *)
    | _ -> assert false

  type exec_sync_opts = { cwd : string;
                          input : str_or_buff ; }

  let exec_sync ?opts cmd =
    let handle = require_module "child_process" in
    match opts with
    | None -> m handle "execSync" [|to_js_str cmd|] |> Js.to_string
    (* TODO Implement this *)
    | _ -> assert false

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
      m raw_js "kill" [|i (Js.string (string_of_signal signal))|]

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


  let raw_js = require_module "http"

  (* type req_opts = *)
  (*   { protocol : string option; *)
  (*     family : ip_family list; *)
  (*     local_address : string option; *)
  (*     connect_to : [`Socket_path of string *)
  (*                  | `Internet of (string * int)]; *)
  (*     method_ : methods; *)
  (*     path : string; *)
  (*     headers : (string * string) list; *)
  (*     auth : string option; *)
  (*     agent : [`Agent of Js.Unsafe.any | `Do_use of bool] option; *)
  (*   } *)

  (* let default_req_opt = *)
  (*   { protocol = Some "http"; family = [Ip4; Ip6]; local_address = None; *)
  (*     method_ = Get; path = "/"; headers = []; auth = None; *)
  (*     agent = option; } *)

  let status_codes =
    (require_module "http") <!> "STATUS_CODES" |> to_json
    |> Yojson.Basic.Util.to_assoc
    |> List.map begin fun (code, message) ->
      (int_of_string code, Yojson.Basic.Util.to_string message)
    end

  class incoming_message raw_js = object

    inherit Stream.readable raw_js

    method http_version = raw_js <!> "httpVersion" |> Js.to_string

    method headers =
      raw_js <!> "headers" |> to_json
      |> Yojson.Basic.Util.to_assoc
      |> List.map (fun (a, b) -> (a, Yojson.Basic.Util.to_string b))

    method trailers = raw_js <!> "trailers" |> to_json

    method raw_headers =
      (raw_js <!> "rawHeaders" : Js.js_string Js.t Js.js_array Js.t)
      |> Js.to_array
      |> Array.map Js.to_string
      |> Array.to_list

    method set_timeout ~msec (f: (unit -> unit)) : incoming_message =
      m raw_js "setTimeout" [|i (msec : int); i !@f|]

    (** Only valid for request obtained from http.Server. *)
    method method_ =
      match raw_js <!> "method" |> Js.to_string |> String.uppercase with
      | "CHECKOUT" -> Checkout | "Connect" -> Connect
      | "COPY" -> Copy | "DELETE" | "GET" -> Get | "HEAD" -> Head
      | "LOCK" -> Lock | "MSEARCH" -> M_search | "MKACTIVITY" -> Mkactivity
      | "MKCOL" -> Mkcol | "MOVE" -> Move | "NOTIFY" -> Notify
      | "OPTIONS" -> Options | "PATCH" -> Patch | "POST" -> Post
      | "PROPFIND" -> Propfind | "PROPPATH" -> Proppath | "PURGE" -> Purge
      | "PUT" -> Put | "REPORT" -> Report | "SEARCH" -> Search
      | "SUBSCRIBE" -> Subscribe | "TRACE" -> Trace | "UNLOCK" -> Unlock
      | "UNSUBSCRIBE" -> Unsubscribe | _ -> assert false

    (** Only valid for request obtained from http.Server, Request URL
        string. This contains only the URL that is present in the actual
        HTTP request *)
    method url = raw_js <!> "url" |> Js.to_string

    method status_code : int = raw_js <!> "statusCode"

    method status_message = raw_js <!> "statusMessage" |> Js.to_string

    method socket = new Net.socket (raw_js <!> "socket")

  end

  class server_response raw_js = object

    inherit Stream.readable raw_js

    method on_finish (f : (unit -> unit)) : unit =
      m raw_js "on" [| to_js_str "finish"; i !@f|]

    method write_continue : unit =
      m raw_js "writeContinue" [||]

    method write_head
        ?status_message
        ~status_code:(status_code : int)
        (headers : (string * string) list) : unit =
      match status_message with
      | None ->
        m raw_js "writeHead" [|i status_code; i (obj_of_alist headers)|]
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
    method write ?(callback : (unit -> unit) option) ?encoding chunk : unit =
      match (chunk, encoding) with
      | (String s, Some e) ->
        m raw_js "write" [|to_js_str s; string_of_encoding e |> to_js_str|]
      | _ -> assert false

    (* method add_trailers *)

    method end_ ?data ?encoding ?(callback : (unit -> unit) option) () : unit =
      match (data, encoding) with
      | (Some (String s), None) -> m raw_js "end" [|to_js_str s|]
      | (Some (String s), Some e) ->
        m raw_js "end" [|to_js_str s; string_of_encoding e |> to_js_str|]
      | _ -> assert false

    (* method finished *)

  end

  class server handler = object

    inherit Events.event

    val raw_js_server =
      m (require_module "http") "createServer" [|i !@handler|]

    (* method on_request (f : ()) *)
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
      handler
        (new incoming_message incoming_msg) (new server_response response)
    in
    new server wrapped_handler

  class client_request raw_js = object

    method on_response (f : (incoming_message -> unit)) : unit =
      let wrapped = fun incoming -> f (new incoming_message incoming) in
      m raw_js "on" [|to_js_str "response"; i !@wrapped|]

    method on_connect
        (f : (server_response -> Net.socket -> Js.Unsafe.any)) : unit =
      ()
  end

  (* let request (f : (incoming_message -> unit)) = *)
  (*   m raw_js "request" [||] *)

  let get url (f : incoming_message -> unit) =
    let wrap_msg = fun raw_msg -> f (new incoming_message raw_msg) in
    new client_request (m raw_js "get" [|to_js_str url; i !@wrap_msg|])

end

module Fs = struct

  type flag = Read | Write | Read_write | Append

  let fs_module = require_module "fs"

  let string_of_flag = function
    | Read -> "r"
    | Write -> "w"
    | _ -> "r"

  class fs_watcher raw_js = object

    inherit Events.event

    method on_change (f : string -> string -> unit) : unit =
      let g = fun e b -> f (Js.to_string e) (Js.to_string b) in
      m raw_js "on" [|to_js_str "change"; i !@g|]

    method close : unit = m raw_js "close" [||]

  end

  (* TODO Add more options *)
  let watch f =
    new fs_watcher (m fs_module "watch" [|to_js_str f|])

  type options = { encoding : encoding; flag : flag; }

  (* let read_file ?opts ~path (callback : Error.error -> string -> unit) : unit = *)
  (*   match opts with *)
  (*   | None -> *)
  (*     m fs_module "readFile" [|to_js_str path; i callback|] *)
  (*   | Some opts -> match opts with *)
  (*     | {encoding = None ; flag = None } -> *)
  (*       m fs_module "readFile" [|to_js_str path; i callback|] *)
  (*     | {encoding = Some e; flag = None } -> *)
  (*       m fs_module "readFile" [|to_js_str path; i e; i callback|] *)
  (*     | _ -> () *)

  class read_stream raw_js = object

    inherit Stream.readable raw_js

    method on_open (f : int -> unit) : unit =
      m raw_js "on" [|to_js_str "open"; i !@f|]

  end

  class write_stream raw_js = object

    inherit Stream.writable raw_js

    method on_open (f : int -> unit) : unit =
      m raw_js "on" [|to_js_str "open"; i !@f|]

    method bytes_written : int = raw_js <!> "bytesWritten"

  end

  (* TODO Get the handle for fd, whatever it is? *)
  type read_stream_opts = { flags : flag;
                            encoding: encoding;
                            (* file_descriptor : *)
                            mode : int;
                            auto_close : bool; }

  let create_read_stream ?opts path =
    m fs_module "createReadStream" [|to_js_str path|]
    |> new read_stream

  let create_write_stream ?opts path =
    m fs_module "createWriteStream" [|to_js_str path|]
    |> new write_stream

  let read_file_sync ?opts file =
    match opts with
    | None ->
      Buffer (new Buffer.buffer (m fs_module "readFileSync" [|to_js_str file|]))
    | Some { encoding = e; flag; } ->
      let ar_arg = [|to_js_str file; i (object%js
                       val encoding = string_of_encoding e |> Js.string
                       val flag = string_of_flag flag |> Js.string
                     end)|]
      in
      String (m fs_module "readFileSync" ar_arg |> Js.to_string)

end

module OS = struct

  type endian = Big_endian | Little_endian

  type times = { user: int; nice : int; sys : int; idle : int; irq : int; }
  type cpu = { model : string; speed : int; times : times; }


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

module V8 = struct

  type stats = { total_heap_size: int;
                 total_heap_size_executable: int;
                 total_physical_size : int;
                 total_available_size : int;
                 used_heap_size : int;
                 heap_size_limit : int; }

  let raw_js = require_module "v8"

  let heap_statistics () =
    let handle = m raw_js "getHeapStatistics" [||] in
    { total_heap_size = handle <!> "total_heap_size";
      total_heap_size_executable = handle <!> "total_heap_size_executable";
      total_physical_size = handle <!> "total_physical_size";
      total_available_size = handle <!> "total_available_size";
      used_heap_size = handle <!> "used_heap_size";
      heap_size_limit = handle <!> "heap_size_limit"; }

  let set_v8_flag s : unit =
    m raw_js "setFlagsFromString" [|to_js_str s|]

end

module Zlib = struct

  let raw_js = require_module "zlib"

  (* class zlib raw = object *)

  (*   method flush *)
  (*   method params *)
  (*   method reset *)
  (*   method create_de *)

  (* end *)

  class deflate raw = object

    inherit Stream.duplex raw

  end

  class deflate_raw raw = object

    inherit Stream.duplex raw

  end

  class gunzip raw = object

    inherit Stream.duplex raw

  end

  class gzip raw = object

    inherit Stream.duplex raw

  end

  class inflate raw = object

    inherit Stream.duplex raw

  end

  class inflate_raw raw = object

    inherit Stream.duplex raw

  end

  class unzip raw = object

    inherit Stream.duplex raw

  end

  let create_deflate () = new deflate (m raw_js "createDeflate" [||])

  let create_deflate_raw () =
    new deflate_raw (m raw_js "createDeflateRaw" [||])

  let create_gunzip () = new gunzip (m raw_js "createGunzip" [||])

  let create_gzip () = new gzip (m raw_js "createGzip" [||])

  let create_inflate () = new inflate (m raw_js "createInflate" [||])

  let create_inflate_raw () =
    new inflate_raw (m raw_js "createInflateRaw" [||])

  let create_unzip () = new unzip (m raw_js "createUnzip" [||])

  (* TODO Write the Async Versions and the options object *)
  let deflate_sync input =
    (match input with
     | String s -> m raw_js "deflateSync" [|to_js_str s|]
     | Buffer b -> m raw_js "deflateSync" [|i b#unsafe_raw|])
    |> new Buffer.buffer

  let deflate_raw_sync input =
    (match input with
     | String s -> m raw_js "deflateRawSync" [|to_js_str s|]
     | Buffer b -> m raw_js "deflateRawSync" [|i b#unsafe_raw|])
    |> new Buffer.buffer

  let gunzip_sync input =
    (match input with
     | String s -> m raw_js "gunzipSync" [|to_js_str s|]
     | Buffer b -> m raw_js "gunzipSync" [|i b#unsafe_raw|])
    |> new Buffer.buffer

  let inflate_sync input =
    (match input with
     | String s -> m raw_js "inflateSync" [|to_js_str s|]
     | Buffer b -> m raw_js "inflateSync" [|i b#unsafe_raw|])
    |> new Buffer.buffer

  let unzip_sync input =
    (match input with
     | String s -> m raw_js "unzipSync" [|to_js_str s|]
     | Buffer b -> m raw_js "unzipSync" [|i b#unsafe_raw|])
    |> new Buffer.buffer

end

(** This module contains utilities for handling and transforming file
    paths. Almost all these methods perform only string
    transformations. The file system is not consulted to check whether
    paths are valid.*)
module Path = struct

  type parsed_path = { root : string;
                       dir : string;
                       base : string;
                       ext: string;
                       name : string; }

  let raw_js = require_module "path"

  let sep = raw_js <!> "sep"

  let delimiter = raw_js <!> "delimiter"

  let normalize s = m raw_js "normalize" [|to_js_str s|] |> Js.to_string

  let join l =
    m raw_js "join" (List.map to_js_str l |> Array.of_list)
    |> Js.to_string

  (** Resolves to to an absolute path.

      If to isn't already absolute from arguments are prepended in right to
      left order, until an absolute path is found. If after using all from
      paths still no absolute path is found, the current working directory
      is used as well. The resulting path is normalized, and trailing
      slashes are removed unless the path gets resolved to the root
      directory. Non-string from arguments are ignored.

      Another way to think of it is as a sequence of cd commands in a
      shell.*)
  let resolve ~from to_ =
    m raw_js "resolve"
      (Array.append
         (List.map to_js_str from |> Array.of_list) [|to_js_str to_|])
    |> Js.to_string

  let is_absolute p = m raw_js "isAbsolute" [|to_js_str p|] |> Js.to_bool

  (** Solve the relative path from from to to.

      At times we have two absolute paths, and we need to derive the
      relative path from one to the other. This is actually the reverse
      transform of resolve. *)
  let relative ~from to_ =
    m raw_js "relative" [|to_js_str from; to_js_str to_|]
    |> Js.to_string

  let dir_name p = m raw_js "dirname" [|to_js_str p|] |> Js.to_string

  let base_name ?ext p =
    (match ext with
     | None -> m raw_js "basename" [|to_js_str p|]
     | Some e -> m raw_js "basename" [|to_js_str p; to_js_str e|])
    |> Js.to_string

  let ext_name p =
    match m raw_js "extname" [|to_js_str p|] |> Js.to_string with
    | "" -> None
    | e -> Some e

  let parse p =
    let h = m raw_js "parse" [|to_js_str p|] in
    let pull key = h <!> key |> Js.to_string in
    { root = pull "root";
      dir = pull "dir";
      base = pull "base";
      ext = pull "ext";
      name = pull "name"; }

  let format {root; dir; base; ext; name} =
    m raw_js "format" [|
      !!(object%js
        val root = to_js_str root
        val dir = to_js_str dir
        val base = to_js_str base
        val ext = to_js_str ext
        val name = to_js_str name
      end)
    |]
    |> Js.to_string

end

module Puny_code = struct

  let raw_js = require_module "punycode"

  let version = raw_js <!> "version"

  (** Converts a Punycode string of ASCII-only symbols to a string of
      Unicode symbols. *)
  let decode s = m raw_js "decode" [|to_js_str s|] |> Js.to_string

  (** Converts a string of Unicode symbols to a Punycode string of
      ASCII-only symbols.*)
  let encode s = m raw_js "encode" [|to_js_str s|] |> Js.to_string

  (** Converts a Punycode string representing a domain name to
      Unicode. Only the Punycoded parts of the domain name will be
      converted, i.e. it doesn't matter if you call it on a string that
      has already been converted to Unicode. *)
  let to_unicode d = m raw_js "toUnicode" [|to_js_str d|] |> Js.to_string

  (** Converts a Unicode string representing a domain name to
      Punycode. Only the non-ASCII parts of the domain name will be
      converted, i.e. it doesn't matter if you call it with a domain
      that's already in ASCII. *)
  let to_ascii d = m raw_js "toASCII" [|to_js_str d|] |> Js.to_string

  let to_ucs2_array s : int array =
    let ucs2 = raw_js <!> "ucs2" in
    m ucs2 "decode" [|to_js_str s|] |> Js.to_array

    [@@ocaml.warning "This isn't implemented correctly"]
  let of_ucs2_array (ar : int array) =
    let ucs2 = raw_js <!> "ucs2" in
    (* Something wrong is happening in this Array.map *)
    let b = m ucs2 "encode" (Array.map i ar) in
    b |> Js.to_string

end

module Util = struct

  type logging_func = (string -> unit)

  (* type inspect_opts = { show_hidden = bool; *)
  (*                       dept : int option; *)
  (*                       colors : bool; *)
  (*                       custom_inspect : bool; } *)

  let raw_js = require_module "util"

  (** This is used to create a function which conditionally writes to
      stderr based on the existence of a NODE_DEBUG environment
      variable. If the section name appears in that environment
      variable, then the returned function will be similar to
      console.error(). If not, then the returned function is a
      no-op.

      You may separate multiple NODE_DEBUG environment variables with
      a comma. For example, NODE_DEBUG=fs,net,tls. *)
  let debug_log s : logging_func = m raw_js "debuglog" [|to_js_str s|]

  let log s : unit = m raw_js "log" [|to_js_str s|]

  (* let inspect ?opts obj = *)
  (*   match opts with *)
  (*   | None -> m raw_js "inspect" obj *)

end

module Cluster = struct

  type s_policy = None | Round_robin

  class worker raw_js = object

    method id : int = raw_js <!> "id"

    (* method process : Child_process.child_proc *)

    (** The boolean worker.suicide lets you distinguish between
        voluntary and accidental exit, the master may choose not to
        respawn a worker based on this value.*)
    method suicide : bool option =
      if raw_js <!> "suicide" = Js.undefined then None
      else Some (raw_js <!> "suicide")

    (* method send ?callback ?handle msg : bool =  *)

    (** This function will kill the worker. In the master, it does
        this by disconnecting the worker.process, and once disconnected,
        killing with signal. In the worker, it does it by disconnecting
        the channel, and then exiting with code 0.

        Causes suicide to be set.*)
    method kill (sig_ : signal option) : unit = match sig_ with
      | None -> m raw_js "kill" [|string_of_signal SIG_TERM |> to_js_str|]
      | Some s -> m raw_js "kill" [|string_of_signal s |> to_js_str|]

    (** In a worker, this function will close all servers, wait for
        the 'close' event on those servers, and then disconnect the IPC
        channel.

        In the master, an internal message is sent to the worker
        causing it to call .disconnect() on itself.

        Causes .suicide to be set.

        Note that after a server is closed, it will no longer accept
        new connections, but connections may be accepted by any other
        listening worker. Existing connections will be allowed to
        close as usual. When no more connections exist, see
        server.close(), the IPC channel to the worker will close
        allowing it to die gracefully.

        The above applies only to server connections, client
        connections are not automatically closed by workers, and
        disconnect does not wait for them to close before exiting.

        Note that in a worker, process.disconnect exists, but it is
        not this function, it is disconnect.

        Because long living server connections may block workers from
        disconnecting, it may be useful to send a message, so
        application specific actions may be taken to close them. It
        also may be useful to implement a timeout, killing a worker if
        the disconnect event has not been emitted after some time. *)
    method disconnect : unit = m raw_js "disconnect" [||]

    (** This function returns true if the worker's process has
        terminated (either because of exiting or being
        signaled). Otherwise, it returns false. *)
    method is_dead = m raw_js "isDead" [||] |> Js.to_bool

    (** This function returns true if the worker is connected to its
        master via its IPC channel, false otherwise. A worker is connected to
        its master after it's been created. It is disconnected after the
        disconnect event is emitted. *)
    method is_connected = m raw_js "isConnected" [||] |> Js.to_bool

    (* method on_message  : unit = *)
    (** Similar to the cluster.on('online') event, but specific to
        this worker. *)
    method on_online (f : (unit -> unit)) : unit =
      m raw_js "on" [|to_js_str "online"; i !@f|]

    (* method on_listening (f : (address -> unit)) : unit = *)
    (*   m raw_js "on" [|to_js_str "listening"; i !@f|] *)

    method on_disconnect (f : (unit -> unit)) : unit =
      m raw_js "on" [|to_js_str "disconnect"; i !@f|]

    method on_exit (f : (int -> signal -> unit)) : unit =
      let wrapped = fun e_code signal -> f e_code (signal_of_string signal) in
      m raw_js "on" [|to_js_str "exit"; i !@wrapped|]

  end

  type settings = { exec_argv : string list;
                    exec_path : string;
                    exec_argv_worker : string list;
                    silent : bool;
                    uid : int;
                    gid : int; }

  class cluster = object

    val raw_js = require_module "cluster"

    method scheduling_policy =
      match raw_js <!> "schedulingPolicy" with
      | 0 -> None | 1 -> Round_robin | _ -> assert false

    method settings =
      let h = raw_js <!> "settings" in
      { exec_argv = h <!> "execArgv" |> to_string_list;
        exec_path = h <!> "exec" |> Js.to_string;
        exec_argv_worker = h <!> "args" |> to_string_list;
        silent = h <!> "silent" |> Js.to_bool;
        uid = h <!> "uid";
        gid = h <!> "gid"; }

    method is_master = m raw_js "isMaster" [||] |> Js.to_bool

    method is_worker = m raw_js "isWorker" [||] |> Js.to_bool

    (** Spawn a new worker process. This can only be called from the
        master process.*)
    method fork (env : (string * string) list option) =
      match env with
      | None -> (m raw_js "fork" [||]) |> new worker
      | Some l -> m raw_js "fork" [| !!(l |> obj_of_alist) |] |> new worker

    (** A reference to the current worker object. Not available in the
        master process.*)
    method worker = new worker (raw_js <!> "worker")

    (** Only available in the master process.

        A worker is removed from cluster.workers after the worker has
        disconnected and exited. The order between these two events
        cannot be determined in advance. However, it is guaranteed
        that the removal from the cluster.workers list happens before
        last 'disconnect' or 'exit' event is emitted. *)
    method workers : (int * worker) list =
      raw_js <!> "workers"
      |> for_each_map ~f:(fun k raw_w -> (int_of_string k, new worker raw_w))

  end

end

(** This module provides utilities for dealing with query strings. *)
module Query_string = struct

  let raw_js = Js.Unsafe.variable "querystring"

  (* TODO Need to add the options object as well *)

  (** Serialize an object to a query string. Optionally override the
      default separator ('&') and assignment ('=') characters. *)
  let stringify ?(sep="&") ?(eq="=") (obj : Js.Unsafe.any) =
    m raw_js "stringify" [|i obj; to_js_str sep; to_js_str eq;|]

  (** Deserialize a query string to an object. Optionally override the
      default separator ('&') and assignment ('=') characters.*)
  let parse ?(sep="&") ?(eq="=") s : Js.Unsafe.any =
    m raw_js "parse" [|to_js_str s; to_js_str sep; to_js_str eq;|]

end

module Udp_datagram = struct


end

module Readline = struct

  class interface raw_js = object

  end

end

module TLS = struct

  let raw_tls_module = require_module "tls"

  let ciphers () = m raw_tls_module "getCiphers" [||] |> to_string_list

  class secure_pair raw_js = object

    inherit Events.event

    method on_secure (f : unit -> unit) : unit =
      m raw_js "on" [|to_js_str "secure"; i !@f|]

  end

  class server raw_js = object

    inherit Net.server raw_js

    (* method on_client_Error  *)

  end
  and socket raw_js = object

    inherit Net.socket raw_js

  end

  (* let create_server *)

end

module Https = struct

  let raw_js = require_module "https"

  let get url (f : Http.incoming_message -> unit) =
    let wrap_msg = fun raw_msg -> f (new Http.incoming_message raw_msg) in
    new Http.client_request (m raw_js "get" [|to_js_str url; i !@wrap_msg|])

end

let ( >|> ) (l : #Stream.duplex) (r : #Stream.duplex) =
  l#pipe (r :> Stream.duplex)

module String_decoder = struct


  class decoder encoding = object

    val raw_js =
      let e = string_of_encoding encoding in
      let do_eval =
        Printf.sprintf
          "new (require(\"string_decoder\")).StringDecoder(\"%s\")" e
      in
      Js.Unsafe.eval_string do_eval

    method end_ : unit = m raw_js "end" [||]

    method write (b : Buffer.buffer) =
      m raw_js "write" [|i b#unsafe_raw|] |> Js.to_string

  end


end
