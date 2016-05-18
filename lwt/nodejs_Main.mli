(** Main loop. *)

val run : unit Lwt.t -> unit
(** [run t] executes the thread [t] until it ends. This statement
    never returns and will exit the program. *)
