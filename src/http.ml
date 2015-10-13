open Internal

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

class type http = object
  method methods : Js.js_string Js.js_array Js.readonly_prop
end

let http () : http Js.t =
  Internal.require (Js.string "http")

let port = Js.number_of_float 8080.0

let handle_request (req: Js.Unsafe.any) (resp : Js.Unsafe.any) =
  Js.Unsafe.meth_call resp "end" [|Js.string "It Works!" |> Js.Unsafe.inject|]

let server =
  Js.Unsafe.meth_call (http ()) "createServer"
    [|Js.Unsafe.inject handle_request|]
