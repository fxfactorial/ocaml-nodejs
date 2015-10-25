(* open Nodejs *)

(* let program () = *)
(*   let express = Express.require () in *)
(*   let app = express##call () in *)
(*   let server = *)
(*     (Http.require ())##createServer_withapp app *)
(*   in *)
(*   let io = (Socket_io.require ())##call_arg server in *)

(*   let port = 8080 in *)

(*   let _ = *)
(*     server##listen port !@(fun () -> print_endline "Server Running") *)
(*   in *)

(*   (\* Routing *\) *)
(*   app##use (express##static !$(Nodejs_globals.__dirname () ^ "/public")); *)

(*   let user_names = object%js end in *)
(*   let num_users = ref 0 in *)
(*   io##on !$"connection" !@ begin fun socket -> *)
(*       let added_user = ref false in *)
(*       socket##on !$"new message" !@ begin fun data -> *)


(*         socket##.broadcast#get##emit *)
(*           !$"new message" *)
(*           (object%js val username = !$"Hello" val message = !$"Bye" end) *)

(*       end *)
(*   end *)

(* let run p = *)
(*   ignore (p ()) *)

(* let () = *)
(*   run program *)

open Nodejs

let m () : <test : Js.js_string Js.t Js.readonly_prop> Js.t =
  object%js
    val test = !$""
  end;;

let () =
  let g = m () in
  let word = g##.test in
  ()
