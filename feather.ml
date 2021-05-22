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
  let exit = ref 0

  let pid = Unix.getpid ()
end

let debug = ref false

type env = (string * string) list

(* TODO: implement [wait] *)

type context = {
  stdin_reader : Unix.file_descr;
  stdout_writer : Unix.file_descr;
  stderr_writer : Unix.file_descr;
  background : bool;
  cwd : string option;
  env : env option;
}

type cmd =
  | Process of string * string list
  | Pipe of cmd * cmd
  | And of cmd * cmd
  | Or of cmd * cmd
  | Sequence of cmd * cmd
  | WriteOutTo of string * cmd
  | AppendOutTo of string * cmd
  | WriteErrTo of string * cmd
  | AppendErrTo of string * cmd
  | ReadInFrom of string * cmd
  | OutToErr of cmd
  | ErrToOut of cmd
  | FilterMap of (string -> string option)

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
  let forward () =
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
  in
  if ctx.background then Thread.run forward else forward ()

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
  let finish () =
    (match snd (Unix.waitpid [] pid) with
    | WEXITED s -> State.exit := s
    | WSIGNALED _ -> ()
    | WSTOPPED _ -> ());
    Unix.close ctx.stdout_writer;
    Unix.close ctx.stderr_writer
  in
  if ctx.background then Thread.run finish else finish ()

let rec eval cmd ctx =
  match cmd with
  | Process (name, args) -> exec name args ctx
  | Pipe (a, b) ->
      let pipe_reader, pipe_writer = Spawn.safe_pipe () in
      eval a
        {
          ctx with
          stdout_writer = pipe_writer;
          stderr_writer = Unix.dup ctx.stderr_writer;
          background = true;
        };
      eval b { ctx with stdin_reader = pipe_reader }
  | And (a, b) -> (
      eval a
        {
          ctx with
          stdout_writer = Unix.dup ctx.stdout_writer;
          stderr_writer = Unix.dup ctx.stderr_writer;
        };
      match !State.exit with
      | 0 -> eval b ctx
      | _ ->
          Unix.close ctx.stdout_writer;
          Unix.close ctx.stderr_writer)
  | Or (a, b) -> (
      eval a
        {
          ctx with
          stdout_writer = Unix.dup ctx.stdout_writer;
          stderr_writer = Unix.dup ctx.stderr_writer;
        };
      match !State.exit with
      | 0 ->
          Unix.close ctx.stdout_writer;
          Unix.close ctx.stderr_writer
      | _ -> eval b ctx)
  | Sequence (a, b) ->
      eval a
        {
          ctx with
          stdout_writer = Unix.dup ctx.stdout_writer;
          stderr_writer = Unix.dup ctx.stderr_writer;
        };
      eval b ctx
  | WriteOutTo (str, cmd) ->
      Unix.close ctx.stdout_writer;
      let stdout_writer =
        Unix.openfile str [ O_WRONLY; O_TRUNC; O_CREAT ] 0o644
      in
      eval cmd { ctx with stdout_writer }
  | AppendOutTo (str, cmd) ->
      Unix.close ctx.stdout_writer;
      let stdout_writer =
        Unix.openfile str [ O_WRONLY; O_APPEND; O_CREAT ] 0o644
      in
      eval cmd { ctx with stdout_writer }
  | WriteErrTo (str, cmd) ->
      let stderr_writer =
        Unix.openfile str [ O_WRONLY; O_TRUNC; O_CREAT ] 0o644
      in
      eval cmd { ctx with stderr_writer }
  | AppendErrTo (str, cmd) ->
      let stderr_writer =
        Unix.openfile str [ O_WRONLY; O_APPEND; O_CREAT ] 0o644
      in
      eval cmd { ctx with stderr_writer }
  | ReadInFrom (str, cmd) ->
      let stdin_reader = Unix.openfile str [ O_RDONLY ] 0 in
      eval cmd { ctx with stdin_reader }
  | OutToErr cmd ->
      Unix.close ctx.stdout_writer;
      let stdout_writer = Unix.dup ctx.stderr_writer in
      eval cmd { ctx with stdout_writer }
  | ErrToOut cmd ->
      Unix.close ctx.stderr_writer;
      let stderr_writer = Unix.dup ctx.stdout_writer in
      eval cmd { ctx with stderr_writer }
  | FilterMap f -> filter_map ~f ctx

let process name args = Process (name, args)

let ( |. ) a b = Pipe (a, b)

let and_ a b = And (a, b)

let or_ a b = Or (a, b)

let sequence a b = Sequence (a, b)

(* Redirection *)

let write_stdout_to str cmd = WriteOutTo (str, cmd)

let append_stdout_to str cmd = AppendOutTo (str, cmd)

let write_stderr_to str cmd = WriteErrTo (str, cmd)

let append_stderr_to str cmd = AppendErrTo (str, cmd)

let read_stdin_from str cmd = ReadInFrom (str, cmd)

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

let stdout_to_stderr cmd = OutToErr cmd

let stderr_to_stdout cmd = ErrToOut cmd

(* === Redirection === *)

let collect_gen ?cwd ?env cmd =
  let stdout_reader, stdout_writer = Unix.pipe () in
  eval cmd
    {
      stdin_reader = Unix.dup Unix.stdin;
      stdout_writer;
      stderr_writer = Unix.dup Unix.stderr;
      background = false;
      cwd;
      env;
    };
  Unix.in_channel_of_descr stdout_reader

let collect_stdout ?cwd ?env cmd =
  let out = In_channel.input_all (collect_gen ?cwd ?env cmd) in
  (* This might be controversial. The alternative is to export a [trim]
     command, that makes it easy to do this manually, but I think this
     is actually less suprising than keeping the newline. *)
  String.chop_suffix out ~suffix:"\n" |> Option.value ~default:out

let collect_lines ?cwd ?env cmd =
  In_channel.input_lines (collect_gen ?cwd ?env cmd)

let map_lines ~f = FilterMap (fun a -> Some (f a))

let filter_lines ~f = FilterMap (fun a -> if f a then Some a else None)

let run' ?cwd ?env ~background cmd =
  Caml.flush_all ();
  eval cmd
    {
      stdin_reader = Unix.dup Unix.stdin;
      stdout_writer = Unix.dup Unix.stdout;
      stderr_writer = Unix.dup Unix.stderr;
      background;
      cwd;
      env;
    }

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

let last_exit () = !State.exit

let devnull = "/dev/null"

let fzf ?cwd ?env cmd =
  let stdout = cmd |. process "fzf" [] |> collect_stdout ?cwd ?env in
  if last_exit () = 0 then Some stdout else None

(* === Signals === *)
let terminate_child_processes () =
  process "pgrep" [ "-P"; Int.to_string State.pid ]
  |> collect_lines |> List.map ~f:Int.of_string
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

    let print cmd = collect_lines cmd |> List.iter ~f:print_endline

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

    let%expect_test "redirection" =
      find "." ~ignore_hidden:true ~kind:`Files ~name:"*.ml"
      |. rg_v {|\.pp\.|} |. sort |> print;
      [%expect {|
        ./example.ml
        ./feather.ml |}]

    let%expect_test "redirection" =
      (* TODO: tests for redirection *)
      ()

    let%expect_test "waitpid should retry on EINTR" =
      process "kill"
        (List.map ~f:Int.to_string [ Caml.Sys.sigurg; Unix.getpid () ])
      |> print;
      [%expect ""]
  end)
