open Printf
open Js
open Pervasives_Js

class type exec_options =
object
end

class type child_process =
object
  method execFile : js_string t -> js_string t js_array t -> exec_options t ->
    (error t opt -> js_string t -> js_string t -> unit) callback -> unit meth
end

let _child_process : child_process t =
  require_module "child_process"

let env_split s =
  try
    let i = String.index s '=' in
    let l = String.length s in
    (String.sub s 0 i, String.sub s (i+1) (l - i - 1))
  with Not_found ->
    ksprintf failwith "Nodejs_ChildProcess.env_split: %S: Cannot split environment binding." s

let jsenv_of_unixenv env =
  let f s =
    let (key, value) = env_split s in
    key, unsafe_string value
  in
  Unsafe.obj (Array.map f env)

let exec_file :
  ?cwd:string -> ?env:string array -> ?encoding:string ->
  ?timeout: int -> ?uid:int -> ?gid:int ->
  string -> string array -> (string * string) Lwt.t =
  fun ?cwd ?env ?encoding ?timeout ?uid ?gid cmd argv ->
    let sleeper, handle = Lwt.wait () in
    let callback error stdout stderr =
      Opt.case error
        (fun () -> Lwt.wakeup handle (to_string stdout, to_string stderr))
        (fun err -> Lwt.wakeup_exn handle (Error err))
    in
    _child_process##execFile
      (string cmd)
      (array_to_js string argv)
      (unsafe_obj_filter [
          maybe_member unsafe_string "cwd" cwd;
          maybe_member jsenv_of_unixenv "env" env;
          maybe_member unsafe_string "encoding" encoding;
          maybe_member Unsafe.inject "timeout" timeout;
          maybe_member Unsafe.inject "uid" uid;
          maybe_member Unsafe.inject "gid" gid;
        ])
      (wrap_callback callback);
    sleeper
