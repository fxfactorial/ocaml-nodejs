(** Child processes. *)

val exec_file :
  ?cwd:string -> ?env:string array -> ?encoding:string ->
  ?timeout:int -> ?uid:int -> ?gid:int ->
  string -> string array -> (string * string) Lwt.t
(** [exec_file program argv] run the [program] with the given argument
    vector [argv] and return its standard output and standard error. *)
