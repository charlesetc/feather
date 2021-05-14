open Base

type cmd

val process : string -> string list -> cmd
(** [process] constructs a new command *)

(* === Basic commands === *)

val cat : string -> cmd

val echo : string -> cmd

val ls : string -> cmd

val sh : string -> cmd

val rg : ?in_:string -> string -> cmd
(** [in_] is the directory that should be rg'd: rg <search> <in>. Without it, it'll filter
    stdin, just rg <search> *)

val rg_v : ?in_:string -> string -> cmd

val find :
  ?include_starting_dir:bool ->
    (** [include_starting_dir]: whether to include the starting directory
        passed into [find]. Defaults to [false], notably different than the unix
        find utility.  *)
  ?ignore_hidden:bool ->
  ?kind:[ `Files | `Directories ] ->
  ?name:string ->
  ?depth:int ->
    (** [depth]: The maximum search depth *)
  string ->
  cmd

val tr : string -> string -> cmd

val tr_d : string -> cmd

val grep : ?in_:string -> string -> cmd

val mkdir : string -> cmd

val mkdir_p : string -> cmd

val pwd : cmd

val less : cmd

val sort : cmd

val uniq : cmd

val shuf : cmd

val head : ?file:string -> int -> cmd

val tail : ?file:string -> int -> cmd

val tail_f : string -> cmd

val cp : string -> string -> cmd

val cp_r : string -> string -> cmd

val mv : string -> string -> cmd

val sed :
  ?g:bool (** defaults to TRUE *) ->
  string (** pattern *) ->
  string (** replacement *) ->
  cmd

val cut : ?d:char (** defaults to a space *) -> int -> cmd

val cut' : ?complement:unit -> ?d:char -> int list -> cmd


(* === Using [cmd]'s in OCaml === *)

val ( |. ) : cmd -> cmd -> cmd
(** [ |. ] is feather's version of a "|" in bash. *)

val and_ : cmd -> cmd -> cmd
(** [ and_ ] is feather's version of a "&&" in bash. See Infix module for more. *)

val  or_ : cmd -> cmd -> cmd
(** [ or_ ] is feather's version of a "||" in bash. See Infix module for more. *)

val sequence : cmd -> cmd -> cmd
(** [ sequence ] is feather's version of a ";" in bash. See Infix module for more. *)

val collect_lines :
  ?cwd:string -> ?env:(string * string) list -> cmd -> string list

val collect_stdout : ?cwd:string -> ?env:(string * string) list -> cmd -> string

val map_lines : f:(string -> string) -> cmd

val filter_lines : f:(string -> bool) -> cmd
(** [map] within a series of pipes will be run in a thread.  *)

val run : ?cwd:string -> ?env:(string * string) list -> cmd -> unit

val run_bg : ?cwd:string -> ?env:(string * string) list -> cmd -> unit
(** Run the process in a thread. Use [wait] to ensure that the parent
 won't exit, subsequently killing the background process. *)

(* Redirection *)

val write_stdout_to : string -> cmd -> cmd

val append_stdout_to : string -> cmd -> cmd

val write_stderr_to : string -> cmd -> cmd

val append_stderr_to : string -> cmd -> cmd

val read_stdin_from : string -> cmd -> cmd

module Infix : sig

  (** Redirect Stdout *)
  val ( > ) : cmd -> string -> cmd

  val ( >> ) : cmd -> string -> cmd

  (** Redirect Stderr *)
  val ( >! ) : cmd -> string -> cmd

  val ( >>! ) : cmd -> string -> cmd

  (** Read file from stdin *)
  val ( < ) : cmd -> string -> cmd

  (** Same as [and_] *)
  val (&&.) : cmd -> cmd -> cmd

  (** Same as [or_] *)
  val (||.) : cmd -> cmd -> cmd

  (** Same as [sequence] *)
  val (->.) : cmd -> cmd -> cmd
end

val stdout_to_stderr : cmd -> cmd

val stderr_to_stdout : cmd -> cmd
(** [stdout_to_stderr] and [stderr_to_stdout] are NOT composable!
    Think of these functions as each creating a new command with the given redirection.

    Applying both will result in no output to either stdout or stderr.
    [flip_stdout_and_stderr] should be easy to write if anyone should need it. *)

(* === Misc === *)

val devnull : string
(** [devnull] is easier to type than "/dev/null" *)

val last_exit : unit -> int
(** [last_exit] returns the exit status of the last child process to have exited *)

val fzf : ?cwd:string -> ?env:(string * string) list -> cmd -> string option
(** [fzf] runs the command, and fuzzy finds the stdout.
   Returns [None] if no item was chosen, [Some str] otherwise

   Note that [fzf] is a way to to run a [cmd] and does not in itself return a
   [cmd]. *)

val debug : bool ref
