open Base
open Stdio
module Sys = Caml.Sys

module Unix = struct
  include Unix

  let rec try_until_no_eintr f =
    try f () with Unix.Unix_error (Unix.EINTR, _, _) -> try_until_no_eintr f

  let with_restart_on_eintr ?(restart = false) f =
    if restart then try_until_no_eintr f else f ()

  let dup = dup ~cloexec:true

  let pipe = pipe ~cloexec:true

  let read ?restart fd buf pos len =
    with_restart_on_eintr ?restart (fun () -> read fd buf pos len)

  let write ?restart fd buf pos len =
    with_restart_on_eintr ?restart (fun () -> write fd buf pos len)

  let waitpid ?(restart = true) wait_flags pid =
    with_restart_on_eintr ~restart (fun () -> waitpid wait_flags pid)
end

module Thread = struct
  include Thread

  let run f =
    let (_ : t) = create f () in
    ()
end

module State = struct
  let pid = Unix.getpid ()
end

let debug = ref false

type env = (string * string) list

(* TODO: implement [wait] *)

type context = {
  stdin_reader : Unix.file_descr;
  stdout_writer : Unix.file_descr;
  stderr_writer : Unix.file_descr;
  cwd : string option;
  env : env option;
}

type cmd =
  | Process of string * string list
  | Of_list of string list
  | Pipe of cmd * cmd
  | And of cmd * cmd
  | Or of cmd * cmd
  | Sequence of cmd * cmd
  | Write_out_to of string * cmd
  | Append_out_to of string * cmd
  | Write_err_to of string * cmd
  | Append_err_to of string * cmd
  | Read_in_from of string * cmd
  | Out_to_err of cmd
  | Err_to_out of cmd
  | Filter_map of (string -> string option)

(* GADT type to accomplish dynamic return types for collect *)
type everything = { stdout : string; stderr : string; status : int }

type _ what_to_collect =
  | Collect_status : int what_to_collect
  | Collect_stdout : string what_to_collect
  | Collect_stderr : string what_to_collect
  | Collect_stdout_stderr : (string * string) what_to_collect
  | Collect_stdout_status : (string * int) what_to_collect
  | Collect_stderr_status : (string * int) what_to_collect
  | Collect_everything : everything what_to_collect

let resolve_in_path prog =
  (* Do not try to resolve in the path if the program is something like
   * ./this.exe *)
  if String.split ~on:'/' prog |> List.length <> 1 then Some prog
  else
    let paths = Sys.getenv "PATH" |> String.split ~on:':' in
    List.map paths ~f:(fun d -> Caml.Filename.concat d prog)
    |> List.find ~f:Caml.Sys.file_exists

let resolve_in_path_exn prog =
  match resolve_in_path prog with
  | None -> failwith (Printf.sprintf "no program in path %s" prog)
  | Some prog -> prog

(* We need a way to iterate over the lines of a file descriptor,
   so that when it is closed, we can stop iterating. This is NOT the case for iterating
   over the lines of an in_channel formed from [Unix.in_channel_of_descr], which will not
   be closed when the file descriptor is. *)
let fd_iter_lines ~f fd =
  let buf = Bytes.create 1 in
  let line = ref [] in
  try
    while true do
      match Unix.read fd buf 0 1 with
      | 0 -> raise End_of_file
      | 1 -> (
          match Bytes.get buf 0 with
          | '\n' ->
              f (String.of_char_list (List.rev !line));
              line := []
          | c -> line := c :: !line)
      | _ -> assert false
    done
  with End_of_file ->
    if List.length !line <> 0 then f (String.of_char_list (List.rev !line))

let filter_map ~f ctx =
  fd_iter_lines ctx.stdin_reader ~f:(fun line ->
      match f line with
      | Some out ->
          let buf = Bytes.of_string out in
          let (_ : int) =
            Unix.write ctx.stdout_writer buf 0 (Bytes.length buf)
          in
          let (_ : int) =
            Unix.write ctx.stdout_writer (Bytes.of_string "\n") 0 1
          in
          ()
      | None -> ());
  Unix.close ctx.stdout_writer

let exec prog args ctx =
  let argv = prog :: args in
  let prog = resolve_in_path_exn prog in
  let cwd : Spawn.Working_dir.t =
    match ctx.cwd with
    | None -> Inherit
    | Some cwd ->
        prerr_endline cwd;
        Path cwd
  in
  let env : Spawn.Env.t option =
    Option.map ctx.env ~f:(fun env ->
        List.map env ~f:(fun (key, value) -> Printf.sprintf "%s=%s" key value)
        |> Spawn.Env.of_list)
  in
  (if !debug then
   let tm = Unix.localtime (Unix.time ()) in
   eprintf "%d-%d-%d %d:%d:%d - %s %s\n" tm.tm_year tm.tm_mon tm.tm_mday
     tm.tm_hour tm.tm_min tm.tm_sec prog
     ("(" ^ String.concat ~sep:" " args ^ ")"));
  let pid =
    Spawn.spawn ~cwd ?env ~stdin:ctx.stdin_reader ~stdout:ctx.stdout_writer
      ~stderr:ctx.stderr_writer ~prog ~argv ()
  in
  let status =
    match snd (Unix.waitpid [] pid) with
    | WEXITED s -> s
    | WSIGNALED s | WSTOPPED s -> 128 + s
    (* using common convention *)
  in
  Unix.close ctx.stdin_reader;
  Unix.close ctx.stdout_writer;
  Unix.close ctx.stderr_writer;
  status

let success_status = 0

let exec_list list ctx =
  let stdout_channel = Unix.out_channel_of_descr ctx.stdout_writer in
  Out_channel.output_lines stdout_channel list;
  Unix.close ctx.stdin_reader;
  Out_channel.close stdout_channel;
  Unix.close ctx.stderr_writer;
  success_status

let rec eval cmd ctx =
  match cmd with
  | Process (name, args) -> exec name args ctx
  | Of_list list -> exec_list list ctx
  | Pipe (a, b) ->
      let pipe_reader, pipe_writer = Spawn.safe_pipe () in
      Thread.run (fun () ->
          eval a
            {
              ctx with
              stdout_writer = pipe_writer;
              stderr_writer = Unix.dup ctx.stderr_writer;
            }
          (* Left side of pipe is executed in background *));
      eval b { ctx with stdin_reader = pipe_reader }
  | And (a, b) ->
      let a_status =
        eval a
          {
            ctx with
            stdout_writer = Unix.dup ctx.stdout_writer;
            stderr_writer = Unix.dup ctx.stderr_writer;
            stdin_reader = Unix.dup ctx.stdin_reader;
          }
      in
      if a_status = success_status then eval b ctx
      else (
        Unix.close ctx.stdout_writer;
        Unix.close ctx.stderr_writer;
        Unix.close ctx.stdin_reader;
        a_status)
  | Or (a, b) ->
      let a_status =
        eval a
          {
            ctx with
            stdout_writer = Unix.dup ctx.stdout_writer;
            stderr_writer = Unix.dup ctx.stderr_writer;
            stdin_reader = Unix.dup ctx.stdin_reader;
          }
      in
      if a_status = success_status then (
        Unix.close ctx.stdout_writer;
        Unix.close ctx.stderr_writer;
        Unix.close ctx.stdin_reader;
        a_status)
      else eval b ctx
  | Sequence (a, b) ->
      ignore
        (eval a
           {
             ctx with
             stdout_writer = Unix.dup ctx.stdout_writer;
             stderr_writer = Unix.dup ctx.stderr_writer;
             stdin_reader = Unix.dup ctx.stdin_reader;
           });
      eval b ctx
  | Write_out_to (str, cmd) ->
      Unix.close ctx.stdout_writer;
      let stdout_writer =
        Unix.openfile str [ O_WRONLY; O_TRUNC; O_CREAT ] 0o644
      in
      eval cmd { ctx with stdout_writer }
  | Append_out_to (str, cmd) ->
      Unix.close ctx.stdout_writer;
      let stdout_writer =
        Unix.openfile str [ O_WRONLY; O_APPEND; O_CREAT ] 0o644
      in
      eval cmd { ctx with stdout_writer }
  | Write_err_to (str, cmd) ->
      let stderr_writer =
        Unix.openfile str [ O_WRONLY; O_TRUNC; O_CREAT ] 0o644
      in
      eval cmd { ctx with stderr_writer }
  | Append_err_to (str, cmd) ->
      let stderr_writer =
        Unix.openfile str [ O_WRONLY; O_APPEND; O_CREAT ] 0o644
      in
      eval cmd { ctx with stderr_writer }
  | Read_in_from (str, cmd) ->
      let stdin_reader = Unix.openfile str [ O_RDONLY ] 0 in
      eval cmd { ctx with stdin_reader }
  | Out_to_err cmd ->
      Unix.close ctx.stdout_writer;
      let stdout_writer = Unix.dup ctx.stderr_writer in
      eval cmd { ctx with stdout_writer }
  | Err_to_out cmd ->
      Unix.close ctx.stderr_writer;
      let stderr_writer = Unix.dup ctx.stdout_writer in
      eval cmd { ctx with stderr_writer }
  | Filter_map f ->
      filter_map ~f ctx;
      0

let process name args = Process (name, args)

let ( |. ) a b = Pipe (a, b)

let and_ a b = And (a, b)

let or_ a b = Or (a, b)

let sequence a b = Sequence (a, b)

(* Redirection *)

let write_stdout_to str cmd = Write_out_to (str, cmd)

let append_stdout_to str cmd = Append_out_to (str, cmd)

let write_stderr_to str cmd = Write_err_to (str, cmd)

let append_stderr_to str cmd = Append_err_to (str, cmd)

let read_stdin_from str cmd = Read_in_from (str, cmd)

module Infix = struct
  let ( &&. ) = and_

  let ( ||. ) = or_

  let ( ->. ) = sequence

  let ( > ) cmd str = write_stdout_to str cmd

  let ( >> ) cmd str = append_stdout_to str cmd

  let ( >! ) cmd str = write_stderr_to str cmd

  let ( >>! ) cmd str = append_stderr_to str cmd

  let ( < ) cmd str = read_stdin_from str cmd
end

let stdout_to_stderr cmd = Out_to_err cmd

let stderr_to_stdout cmd = Err_to_out cmd

(* === Collection facilities === *)

let status = Collect_status

let stdout = Collect_stdout

let stderr = Collect_stderr

let stdout_and_stderr = Collect_stdout_stderr

let stdout_and_status = Collect_stdout_status

let stderr_and_status = Collect_stderr_status

let everything = Collect_everything

(* Take a file descriptor and read everything into a single string *)
let collect_into_string fd =
  let out = In_channel.input_all (Unix.in_channel_of_descr fd) in
  (* This might be controversial. The alternative is to export a [trim]
     command, that makes it easy to do this manually, but I think this
     is actually less surprising than keeping the newline. *)
  String.chop_suffix_if_exists out ~suffix:"\n"

(* Launch the command and collect expected channels *)
let collect (type a) ?cwd ?env (what_to_collect : a what_to_collect) cmd : a =
  let eval ?(stdout_writer = Unix.dup Unix.stdout)
      ?(stderr_writer = Unix.dup Unix.stderr) () =
    eval cmd
      {
        stdin_reader = Unix.dup Unix.stdin;
        stdout_writer;
        stderr_writer;
        cwd;
        env;
      }
  in

  match what_to_collect with
  | Collect_status -> eval ()
  | Collect_stdout ->
      let stdout_reader, stdout_writer = Unix.pipe () in
      let (_ : int) = eval ~stdout_writer () in
      let stdout = collect_into_string stdout_reader in
      stdout
  | Collect_stderr ->
      let stderr_reader, stderr_writer = Unix.pipe () in
      let (_ : int) = eval ~stderr_writer () in
      let stderr = collect_into_string stderr_reader in
      stderr
  | Collect_stdout_status ->
      let stdout_reader, stdout_writer = Unix.pipe () in
      let status = eval ~stdout_writer () in
      let stdout = collect_into_string stdout_reader in
      (stdout, status)
  | Collect_stderr_status ->
      let stderr_reader, stderr_writer = Unix.pipe () in
      let status = eval ~stderr_writer () in
      let stderr = collect_into_string stderr_reader in
      (stderr, status)
  | Collect_stdout_stderr ->
      let stdout_reader, stdout_writer = Unix.pipe () in
      let stderr_reader, stderr_writer = Unix.pipe () in
      let (_ : int) = eval ~stdout_writer ~stderr_writer () in
      let stdout = collect_into_string stdout_reader in
      let stderr = collect_into_string stderr_reader in
      (stdout, stderr)
  | Collect_everything ->
      let stdout_reader, stdout_writer = Unix.pipe () in
      let stderr_reader, stderr_writer = Unix.pipe () in
      let status = eval ~stdout_writer ~stderr_writer () in
      let stdout = collect_into_string stdout_reader in
      let stderr = collect_into_string stderr_reader in
      { status; stdout; stderr }

let lines = String.split_lines

let map_lines ~f = Filter_map (fun a -> Some (f a))

let filter_lines ~f = Filter_map (fun a -> if f a then Some a else None)

let run' ?cwd ?env ~background cmd =
  Caml.flush_all ();
  let go () =
    eval cmd
      {
        stdin_reader = Unix.dup Unix.stdin;
        stdout_writer = Unix.dup Unix.stdout;
        stderr_writer = Unix.dup Unix.stderr;
        cwd;
        env;
      }
  in
  if background then Thread.run go else go () |> ignore

let run_bg ?cwd ?env = run' ?cwd ?env ~background:true

let run ?cwd ?env = run' ?cwd ?env ~background:false

(* === Common Unix commands === *)
let ls s = process "ls" [ s ]

let find ?(include_starting_dir = false) ?(ignore_hidden = false)
    ?(kind : [ `Directories | `Files ] option) ?name ?depth directory =
  let args = [ directory ] in
  let args =
    match kind with
    | Some `Files -> args @ [ "-type"; "f" ]
    | Some `Directories -> args @ [ "-type"; "d" ]
    | None -> args
  in
  let args =
    args
    @ ([
         Option.map depth ~f:(fun depth -> [ "-maxdepth"; Int.to_string depth ]);
         Option.map name ~f:(fun name -> [ "-name"; name ]);
         (if ignore_hidden then Some [ "-not"; "-path"; "*/\\.*" ] else None);
         (if include_starting_dir then None
         else Some [ "-not"; "-path"; directory ]);
       ]
      |> List.filter_opt |> List.concat)
  in
  process "find" args

let sh s = process "sh" [ "-c"; s ]

let rg ?in_ regex = process "rg" (List.filter_opt [ Some regex; in_ ])

let rg_v ?in_ regex =
  process "rg" ([ "-v" ] @ List.filter_opt [ Some regex; in_ ])

let grep ?in_ regex = process "grep" (List.filter_opt [ Some regex; in_ ])

let cat file = process "cat" [ file ]

let less = process "less" []

let mkdir dir = process "mkdir" [ dir ]

let mkdir_p dir = process "mkdir" [ "-p"; dir ]

let sort = process "sort" []

let uniq = process "uniq" []

let shuf = process "shuf" []

let head ?file n =
  process "head" (List.filter_opt [ Some "-n"; Some (Int.to_string n); file ])

let tail ?file n =
  process "tail" (List.filter_opt [ Some "-n"; Some (Int.to_string n); file ])

let tail_f file = process "tail" [ "-f"; file ]

let echo s = process "echo" [ s ]

let cut' ?complement ?(d = ' ') fs =
  process
    (match resolve_in_path "gcut" with Some _ -> "gcut" | None -> "cut")
    ([
       "-f";
       List.map fs ~f:Int.to_string |> String.concat ~sep:",";
       "-d";
       Char.to_string d;
     ]
    @ List.filter_opt [ Option.map complement ~f:(fun () -> "--complement") ])

let cut ?d f = cut' ?d [ f ]

let cp src dst = process "cp" [ src; dst ]

let cp_r src dst = process "cp" [ "-r"; src; dst ]

let mv src dst = process "mv" [ src; dst ]

(* TODO: Refactor to not spawn a process. Potentially applicable to:
   - pwd
   - cp
   - mv
   - ls
   - echo

   And probably possible but harder:
   - sort
   - uniq
   - shuf

   N.B if this does end up getting implemented, make sure that [debug] still
   works.
 *)
let pwd = process "pwd" []

let sed ?(g = true) pattern replace =
  process "sed" [ ("s/" ^ pattern ^ "/" ^ replace ^ if g then "/g" else "/") ]

let tr from_ to_ = process "tr" [ from_; to_ ]

let tr_d chars = process "tr" [ "-d"; chars ]

(* === Misc === *)

let of_list l = Of_list l

let devnull = "/dev/null"

let fzf ?cwd ?env cmd =
  let stdout, status =
    cmd |. process "fzf" [] |> collect ?cwd ?env stdout_and_status
  in
  if status = 0 then Some stdout else None

(* === Signals === *)
let terminate_child_processes () =
  process "pgrep" [ "-P"; Int.to_string State.pid ]
  |> collect stdout |> lines |> List.map ~f:Int.of_string
  |> List.iter ~f:(fun pid ->
         try Unix.kill Sys.sigterm pid
         with Unix.Unix_error (Unix.ESRCH, _, _) ->
           (* [Unix.ERSCH] is raised when a process cannot be found, which
            * means that the child has already terminated, so it should
            * be safe to ignore this exception. *)
           ())

let () = Caml.at_exit terminate_child_processes

(* === Tests === *)

let%test_module _ =
  (module struct
    open Infix

    let print cmd = cmd |> collect stdout |> lines |> List.iter ~f:print_endline

    let%expect_test _ =
      echo "hi\n" |> print;
      [%expect "hi"]

    let%expect_test _ =
      echo {|
hi
you
there
hi
|} |. sort |. uniq |. rg "h" |. sed "^" "<> "
      |> print;
      [%expect {|
    <> hi
    <> there |}]

    let%expect_test _ =
      let text = {|
0 hi,2 a
1 you,3 b
|} in
      echo text |. cut 1 |> print;
      [%expect {|
        0
        1 |}];
      echo text |. cut ~d:',' 2 |> print;
      [%expect {|
        2 a
        3 b |}];
      echo text |. cut' [ 1; 3 ] |> print;
      [%expect {|
        0 a
        1 b |}];
      (* complement relies on gnu cut *)
      echo text |. cut' ~complement:() [ 1; 3 ] |> print;
      [%expect {|
        hi,2
        you,3 |}];
      echo text
      |. map_lines ~f:(fun line ->
             String.split line ~on:' '
             |> List.filter ~f:(fun x -> String.length x = 1)
             |> List.rev |> String.concat ~sep:"--")
      |> print;
      [%expect {|
        a--0
        b--1 |}]

    let%expect_test _ =
      process "false" [] &&. echo "test" |> print;
      [%expect ""]

    let%expect_test _ =
      process "true" [] &&. echo "test" |> print;
      [%expect "test"]

    let%expect_test _ =
      process "false" [] ||. echo "test" |> print;
      [%expect "test"]

    let%expect_test _ =
      process "true" [] ||. echo "test" |> print;
      [%expect ""]

    let%expect_test _ =
      process "false" [] ||. process "true" [] &&. echo "test" |> print;
      [%expect "test"]

    let%expect_test _ =
      process "true" [] &&. process "false" [] ||. echo "test" |> print;
      [%expect "test"]

    let%expect_test _ =
      process "false" [] ||. process "false" [] &&. echo "test" |> print;
      [%expect ""]

    let%expect_test _ =
      process "true" [] &&. process "false" [] ||. echo "test" |> print;
      [%expect "test"]

    let%expect_test _ =
      echo "test" |. process "true" [] ->. cat "-" |> print;
      [%expect "test"]

    let%expect_test _ =
      echo "test" |. process "false" [] ->. cat "-" |> print;
      [%expect "test"]

    let%expect_test "find" =
      find "." ~ignore_hidden:true ~kind:`Files ~name:"*.ml"
      |. rg_v {|\.pp\.|} |. sort |> print;
      [%expect {|
        ./example.ml
        ./feather.ml |}]

    let%expect_test "waitpid should retry on EINTR" =
      process "kill"
        (List.map ~f:Int.to_string [ Caml.Sys.sigurg; Unix.getpid () ])
      |> print;
      [%expect ""]

    let%expect_test "redirection/collection" =
      let print cmd =
        let stdout, stderr = cmd |> collect stdout_and_stderr in
        printf "== Stdout ==\n%s\n== Stderr ==\n%s\n" stdout stderr
      in
      echo "test" |> print;
      [%expect {|
        == Stdout ==
        test
        == Stderr == |}];
      echo "test1" |> stdout_to_stderr &&. echo "test2" |> print;
      [%expect
        {|
        == Stdout ==
        test2
        == Stderr ==
        test1 |}];
      echo "test1" |> stdout_to_stderr &&. echo "test2" |> stderr_to_stdout
      |> print;
      [%expect
        {|
        == Stdout ==
        test1
        test2
        == Stderr ==
        |}]

    let%expect_test "of_list" =
      of_list [ "one"; "two"; "three" ] |. sort |> print;
      [%expect {|
        one
        three
        two |}]
  end)
