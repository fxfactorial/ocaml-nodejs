type http_methods = Checkout
                  | Connect
                  | Copy
                  | Delete
                  | Get
                  | Head
                  | Lock
                  | M_search
                  | Merge
                  | Mk_activity
                  | Mk_calendar
                  | Mk_col
                  | Move
                  | Notify
                  | Options
                  | Patch
                  | Post
                  | Propfind
                  | Proppatch
                  | Purge
                  | Put
                  | Report
                  | Search
                  | Subscribe
                  | Trace
                  | Unlock
                  | Unsubscribe

let string_of_method = function
  | Checkout    -> Js.string "checkout"
  | Connect     -> Js.string "connect"
  | Copy        -> Js.string "copy"
  | Delete-> Js.string "delete"
  | Get-> Js.string "get"
  | Head -> Js.string "head"
  | Lock -> Js.string "lock"
  | M_search -> Js.string "msearch"
  | Merge -> Js.string "merge"
  | Mk_activity -> Js.string "mkactivity"
  | Mk_calendar -> Js.string "mkcalendar"
  | Mk_col -> Js.string "mkcol"
  | Move-> Js.string "move"
  | Notify -> Js.string "notify"
  | Options -> Js.string "options"
  | Patch -> Js.string "patch"
  | Post -> Js.string "post"
  | Propfind -> Js.string "propfind"
  | Proppatch -> Js.string "proppatch"
  | Purge -> Js.string "purge"
  | Put-> Js.string "put"
  | Report -> Js.string "report"
  | Search -> Js.string "search"
  | Subscribe -> Js.string "subscribe"
  | Trace -> Js.string "trace"
  | Unlock -> Js.string "unlock"
  | Unsubscribe -> Js.string "unsubscribe"

class type incoming_message = object
  method httpVersion : Js.js_string Js.readonly_prop
end

class type server_response = object
  method url : Js.js_string Js.readonly_prop
  (* How do I say arbtriary JS Object <..> Js.t*)
  method writeHead : int -> Js.Unsafe.any -> unit Js.meth
  method end_ : Js.js_string Js.t -> unit Js.meth
end

class type client_request = object
  method flush_headers : unit -> unit Js.meth
  method write : Js.js_string -> Js.json -> unit Js.meth
end

class type server = object
  method listen : int -> (unit -> unit) Js.callback -> unit Js.meth
  method close : (unit -> unit) Js.callback -> unit Js.meth
  method address :
    unit -> <address: Js.js_string Js.t Js.readonly_prop;
             family : Js.js_string Js.t Js.readonly_prop;
             port: Js.js_string Js.t Js.readonly_prop> Js.t Js.meth
end

class type http = object
  method methods : Js.js_string Js.js_array Js.readonly_prop
  method createServer :
    (incoming_message Js.t -> server_response Js.t -> unit) Js.callback ->
    server Js.t Js.meth
end

let require () : http Js.t =
  Internal.require (Js.string "http")
