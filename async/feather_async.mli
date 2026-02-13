open Async

(** Feather_async provides functions to run Feather cmd's within an Async context. *)

val run : ?cwd:string -> ?env:(string * string) list -> Feather.cmd -> unit Deferred.t

val collect
  :  ?cwd:string
  -> ?env:(string * string) list
  -> 'a Feather.what_to_collect
  -> Feather.cmd
  -> 'a Deferred.t

val fzf
  :  ?cwd:string
  -> ?env:(string * string) list
  -> Feather.cmd
  -> string option Deferred.t

val run_in_background : [ `Use_don't_wait_for ]
[@@alert
  run_bg
    {|  [Feather_async.run_in_background] intentionally shadows [Feather.run_in_background].

          When using [Feather_async], use [Async.don't_wait_for] to run things
          asynchronously. |}]

val collect_in_background : [ `Use_don't_wait_for ]
[@@alert
  run_bg
    {|  [Feather_async.collect_in_background] intentionally shadows [Feather.collect_in_background].

          When using [Feather_async], use [Async.don't_wait_for] to run things
          asynchronously. |}]
