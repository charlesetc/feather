(** {2 {b Creating and combining Feather commands}} *)

type cmd

val process : string -> string list -> cmd
(** [process] constructs a new [cmd] that can be run with [run] or [collect] *)

val ( |. ) : cmd -> cmd -> cmd
(** [ |. ] is Feather's version of a "|" in bash; pipe the first process's
    stdout to the next's stdin. *)

val and_ : cmd -> cmd -> cmd
(** [ and_ ] is feather's version of a "&&" in bash. See Infix module for more.
*)

val or_ : cmd -> cmd -> cmd
(** [ or_ ] is feather's version of a "||" in bash. See Infix module for more.
*)

val sequence : cmd -> cmd -> cmd
(** [ sequence ] is feather's version of a ";" in bash. See Infix module for
    more. *)

(** {2 {b Running Feather commands}} *)

val run : ?cwd:string -> ?env:(string * string) list -> cmd -> unit
(** Run a command without collecting anything *)

type 'a what_to_collect
(** The type that determines what should be returned by {!collect} *)

(** Various collection possibilities, to be used with {!collect} *)

val stdout : string what_to_collect
val stderr : string what_to_collect
val status : int what_to_collect
val stdout_and_stderr : (string * string) what_to_collect
val stdout_and_status : (string * int) what_to_collect
val stderr_and_status : (string * int) what_to_collect

type everything = { stdout : string; stderr : string; status : int }

val everything : everything what_to_collect

val collect :
  ?cwd:string -> ?env:(string * string) list -> 'a what_to_collect -> cmd -> 'a
(** [ collect col cmd ] runs [cmd], collecting the outputs specified by [col]
    along the way and returning them. The return type depends on what is
    collected. *)

type 'a background_process

val run_in_background :
  ?cwd:string -> ?env:(string * string) list -> cmd -> unit background_process

val collect_in_background :
  ?cwd:string ->
  ?env:(string * string) list ->
  'a what_to_collect ->
  cmd ->
  'a background_process
(** [collect_in_background] and [run_in_background] run the command in a thread.

    Use [wait] to wait for the process to finish (and retreive whatever you
    collected). *)

val wait : 'a background_process -> 'a
(** [wait] for the result of [run_in_background] or [collect_in_background]. *)

val wait_all : unit -> unit
(** Wait for all processes started in the background. *)

(** {2 {b Commands to insert OCaml within a Feather pipeline}} *)

val map_lines : f:(string -> string) -> cmd
(** [map_lines] within a sequence of pipes will be run with a thread. Same goes
    for [filter_lines], [mapi_lines], etc. *)

val filter_lines : f:(string -> bool) -> cmd
val mapi_lines : f:(string -> int -> string) -> cmd
val filteri_lines : f:(string -> int -> bool) -> cmd
val filter_map_lines : f:(string -> string option) -> cmd
val filter_mapi_lines : f:(string -> int -> string option) -> cmd

(** {2 {b File redirection}} *)

val write_stdout_to : string -> cmd -> cmd
val append_stdout_to : string -> cmd -> cmd
val write_stderr_to : string -> cmd -> cmd
val append_stderr_to : string -> cmd -> cmd
val read_stdin_from : string -> cmd -> cmd
val stdout_to_stderr : cmd -> cmd

val stderr_to_stdout : cmd -> cmd
(** [stdout_to_stderr] and [stderr_to_stdout] are NOT composable! Think of these
    functions as each creating a new command with the given redirection.

    Applying both will result in no output to either stdout or stderr.
    [flip_stdout_and_stderr] should be easy to write if anyone should need it.
*)

module Infix : sig
  val ( |. ) : cmd -> cmd -> cmd
  (** Same as the top-level [ |. ] pipe operator *)

  val ( > ) : cmd -> string -> cmd
  (** Redirect Stdout *)

  val ( >> ) : cmd -> string -> cmd

  val ( >! ) : cmd -> string -> cmd
  (** Redirect Stderr *)

  val ( >>! ) : cmd -> string -> cmd

  val ( < ) : cmd -> string -> cmd
  (** Read file from stdin *)

  val ( &&. ) : cmd -> cmd -> cmd
  (** Same as [and_] *)

  val ( ||. ) : cmd -> cmd -> cmd
  (** Same as [or_] *)

  val ( ->. ) : cmd -> cmd -> cmd
  (** Same as [sequence]

      [->.] binds more tightly than [|.] so parentheses should be used when
      chaining the two. *)

  val ( <<< ) : cmd -> string -> cmd
  (** Here-string: feed a string directly to a command's stdin without spawning
      a subprocess. Analogous to bash's [<<<]. *)
end

(** {2 {b Built-in commands}} *)

val ls : string -> cmd

val find :
  ?include_starting_dir:bool ->
  ?ignore_hidden:bool ->
  ?kind:[ `Files | `Directories ] ->
  ?name:string ->
  ?depth:int ->
  string ->
  cmd
(** [find] lists files and/or directories, optionally filtering by name.

    [?depth]: The maximum search depth, defaults to infinity.

    [?include_starting_dir]: whether to include the starting directory passed
    into [find]. Defaults to [false], notably different than the unix find
    utility. *)

val sh : string -> cmd

val rg : ?in_:string -> string -> cmd
(** [in_] is the directory that should be rg'd: rg <search> <in>. Without it,
    it'll filter stdin, just rg <search> *)

val rg_v : ?in_:string -> string -> cmd
val grep : ?in_:string -> string -> cmd
val cat : string -> cmd
val less : cmd
val mkdir : string -> cmd
val mkdir_p : string -> cmd
val sort : cmd
val uniq : cmd
val shuf : cmd
val head : ?file:string -> int -> cmd
val tail : ?file:string -> int -> cmd
val tail_f : string -> cmd
val echo : string -> cmd
val cut' : ?complement:unit -> ?d:char -> int list -> cmd
val cut : ?d:char (** defaults to a space *) -> int -> cmd
val cp : string -> string -> cmd
val cp_r : string -> string -> cmd
val mv : string -> string -> cmd
val pwd : cmd

val sed :
  ?g:bool (** defaults to TRUE *) ->
  string (** pattern *) ->
  string (** replacement *) ->
  cmd

val tr : string -> string -> cmd
val tr_d : string -> cmd

(** {2 {b Misc}} *)

val of_list : string list -> cmd
(** [of_list] emulates a Feather.cmd, each item in the list becomes a line on
    stdout. *)

val lines : string -> string list
(** [lines] splits a string into the list of its lines *)

val devnull : string
(** [devnull] is easier to type than "/dev/null" *)

val fzf : ?cwd:string -> ?env:(string * string) list -> cmd -> string option
(** [fzf] runs the command, and fuzzy finds the stdout. Returns [None] if no
    item was chosen, [Some str] otherwise

    Note that [fzf] is a way to to run a [cmd] and does not in itself return a
    [cmd]. *)

val debug : bool ref
