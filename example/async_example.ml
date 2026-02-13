open! Base
open! Async
open! Stdio
open! Feather
open! Feather_async
open! Feather.Infix

let main () : unit Deferred.t =
  let to_echo = "hello world\nwow\ncool" in
  let%bind stdout, status = echo to_echo |. sort |> collect stdout_and_status in
  print_s [%message "result" (stdout |> lines : string list) (status : int)];
  match%bind ls "/tmp" < devnull |> fzf with
  | Some s -> echo [%string "hi there: %{s}"] |> run
  | None -> echo "got nothing" |> run
;;

let () =
  Command_unix.run
  @@ Command.async
       ~summary:"example feather async command"
       (Command.Param.return (fun () -> main ()))
;;
