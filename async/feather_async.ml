open! Base
open! Async

let run_in_background = `Use_don't_wait_for
let collect_in_background = `Use_don't_wait_for
let run ?cwd ?env cmd = In_thread.run (fun () -> Feather.run ?cwd ?env cmd)
let fzf ?cwd ?env cmd = In_thread.run (fun () -> Feather.fzf ?cwd ?env cmd)

let collect ?cwd ?env what_to_collect cmd =
  In_thread.run (fun () -> Feather.collect ?cwd ?env what_to_collect cmd)
