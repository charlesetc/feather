open Core
open Stdio
module Sys = Caml.Sys

module Unix = struct
  include Unix

  let dup = dup ~close_on_exec:true

  let pipe = pipe ~close_on_exec:true
end

module Thread = struct
  include Thread

  let run f =
    let (_ : t) = create ~on_uncaught_exn:`Kill_whole_process f () in
    ()
end

module State = struct
  let exit = ref 0

  let pid = Unix.getpid ()
end

let debug = ref false

type env = (string * string) list

(* TODO: implement [wait] *)

(* TODO: Refactor to use a record *)
type cmd =
  stdin_reader:Unix.File_descr.t ->
  stdout_writer:Unix.File_descr.t ->
  stderr_writer:Unix.File_descr.t ->
  background:bool ->
  cwd:string option ->
  env:env option ->
  unit

let resolve_in_path prog =
  (* Do not try to resolve in the path if the program is something like
   * ./this.exe *)
  if String.split ~on:'/' prog |> List.length <> 1 then prog
  else
    let paths = Sys.getenv "PATH" |> String.split ~on:':' in
    match
      List.map paths ~f:(fun d -> Caml.Filename.concat d prog)
      |> List.find ~f:Caml.Sys.file_exists
    with
    | None -> failwith (Printf.sprintf "no program in path %s" prog)
    | Some prog -> prog

let process prog args ~stdin_reader ~stdout_writer ~stderr_writer ~background
    ~cwd ~env =
  let argv = prog :: args in
  let prog = resolve_in_path prog in
  let cwd : Spawn.Working_dir.t =
    match cwd with
    | None -> Inherit
    | Some cwd ->
        prerr_endline cwd;
        Path cwd
  in
  let env : Spawn.Env.t option =
    Option.map env ~f:(fun env ->
        List.map env ~f:(fun (key, value) -> sprintf "%s=%s" key value)
        |> Spawn.Env.of_list)
  in
  if !debug then
    eprintf "%s - %s %s\n"
      (Time.to_string (Time.now ()))
      prog
      (List.to_string ~f:Fn.id args);
  let pid =
    Spawn.spawn ~cwd ?env ~stdin:stdin_reader ~stdout:stdout_writer
      ~stderr:stderr_writer ~prog ~argv ()
    |> Pid.of_int
  in
  let finish () =
    ( match Unix.waitpid pid with
    | Ok () -> ()
    | Error (`Exit_non_zero s) -> State.exit := s
    | Error (`Signal _) -> () );
    Unix.close stdout_writer
  in
  if background then Thread.run finish else finish ()

let ( |. ) a b =
  let pipe_reader, pipe_writer = Spawn.safe_pipe () in
  fun ~stdin_reader ~stdout_writer ~stderr_writer ~background ~cwd ~env ->
    a ~stdin_reader ~stdout_writer:pipe_writer ~stderr_writer ~background:true
      ~cwd ~env;
    b ~stdin_reader:pipe_reader ~stdout_writer ~stderr_writer ~background ~cwd
      ~env

let collect_gen ?cwd ?env cmd =
  let stdout_reader, stdout_writer = Unix.pipe () in
  cmd ~stdin_reader:(Unix.dup Unix.stdin) ~stdout_writer
    ~stderr_writer:(Unix.dup Unix.stderr) ~background:false ~cwd ~env;
  Unix.in_channel_of_descr stdout_reader

let collect_stdout ?cwd ?env cmd =
  let out = In_channel.input_all (collect_gen ?cwd ?env cmd) in
  (* This might be controversial. The alternative is to export a [trim]
     command, that makes it easy to do this manually, but I think this
     is actually less suprising than keeping the newline. *)
  String.chop_suffix out ~suffix:"\n" |> Option.value ~default:out

let collect_lines ?cwd ?env cmd =
  In_channel.input_lines (collect_gen ?cwd ?env cmd)

(* We need a way to iterate over the lines of a file descriptor,
   so that when it is closed, we can stop iterating. This is NOT the case for iterating
   over the lines of an in_channel formed from [Unix.in_channel_of_descr], which will not
   be closed when the file descriptor is. *)
let fd_iter_lines ~f fd =
  let buf = Bytes.create 1 in
  let line = ref [] in
  try
    while true do
      match
        let read = Unix.read fd ~buf ~pos:0 ~len:1 in
        if read = 0 then raise End_of_file;
        assert (read = 1);
        Bytes.get buf 0
      with
      | '\n' ->
          f (String.of_char_list (List.rev !line));
          line := []
      | c -> line := c :: !line
    done
  with End_of_file ->
    if List.length !line <> 0 then f (String.of_char_list (List.rev !line))

let filter_map ~f ~stdin_reader ~stdout_writer ~stderr_writer:_ ~background
    ~cwd:_ ~env:_ =
  let forward () =
    fd_iter_lines stdin_reader ~f:(fun line ->
        match f line with
        | Some out ->
            let buf = Bytes.of_string out in
            let (_ : int) =
              Unix.write stdout_writer ~buf ~pos:0 ~len:(Bytes.length buf)
            in
            let (_ : int) =
              Unix.write stdout_writer ~buf:(Bytes.of_string "\n") ~pos:0 ~len:1
            in
            ()
        | None -> ());
    Unix.close stdout_writer
  in
  if background then Thread.run forward else forward ()

let map_lines ~f ~stdin_reader ~stdout_writer ~stderr_writer ~background ~cwd
    ~env =
  filter_map ~stdin_reader ~stdout_writer ~stderr_writer ~background ~cwd ~env
    ~f:(fun a -> Some (f a))

let filter_lines ~f ~stdin_reader ~stdout_writer ~stderr_writer ~background ~cwd
    ~env =
  filter_map ~stdin_reader ~stdout_writer ~stderr_writer ~background ~cwd ~env
    ~f:(fun a -> if f a then Some a else None)

let run' ?cwd ?env ~background cmd =
  Caml.flush_all ();
  cmd ~stdin_reader:(Unix.dup Unix.stdin) ~stdout_writer:(Unix.dup Unix.stdout)
    ~stderr_writer:(Unix.dup Unix.stderr) ~background ~cwd ~env

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
    @ ( [
          Option.map depth ~f:(fun depth -> [ "-maxdepth"; Int.to_string depth ]);
          Option.map name ~f:(fun name -> [ "-name"; name ]);
          (if ignore_hidden then Some [ "-not"; "-path"; "*/\\.*" ] else None);
          ( if include_starting_dir then None
          else Some [ "-not"; "-path"; directory ] );
        ]
      |> List.filter_opt |> List.concat )
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
  process "cut"
    ( [
        "-f";
        List.map fs ~f:Int.to_string |> String.concat ~sep:",";
        "-d";
        Char.to_string d;
      ]
    @ List.filter_opt [ Option.map complement ~f:(fun () -> "--complement") ] )

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

(* === Redirection === *)

let write_stdout_to str cmd ~stdin_reader ~stdout_writer ~stderr_writer
    ~background ~cwd ~env =
  Unix.close stdout_writer;
  let stdout_writer =
    Unix.openfile str ~mode:[ O_WRONLY; O_TRUNC; O_CREAT ] ~perm:0o644
  in
  cmd ~stdin_reader ~stdout_writer ~stderr_writer ~background ~cwd ~env

let append_stdout_to str cmd ~stdin_reader ~stdout_writer ~stderr_writer
    ~background ~cwd ~env =
  Unix.close stdout_writer;
  let stdout_writer =
    Unix.openfile str ~mode:[ O_WRONLY; O_APPEND; O_CREAT ] ~perm:0o644
  in
  cmd ~stdin_reader ~stdout_writer ~stderr_writer ~background ~cwd ~env

let write_stderr_to str cmd ~stdin_reader ~stdout_writer ~stderr_writer:_
    ~background ~cwd ~env =
  let stderr_writer =
    Unix.openfile str ~mode:[ O_WRONLY; O_TRUNC; O_CREAT ] ~perm:0o644
  in
  cmd ~stdin_reader ~stdout_writer ~stderr_writer ~background ~cwd ~env

let append_stderr_to str cmd ~stdin_reader ~stdout_writer ~stderr_writer:_
    ~background ~cwd ~env =
  let stderr_writer =
    Unix.openfile str ~mode:[ O_WRONLY; O_APPEND; O_CREAT ] ~perm:0o644
  in
  cmd ~stdin_reader ~stdout_writer ~stderr_writer ~background ~cwd ~env

let read_stdin_from str cmd ~stdin_reader:_ ~stdout_writer ~stderr_writer
    ~background ~cwd ~env =
  let stdin_reader = Unix.openfile str ~mode:[ O_RDONLY ] ~perm:0 in
  cmd ~stdin_reader ~stdout_writer ~stderr_writer ~background ~cwd ~env

module File_redirection_infix = struct
  let ( > ) cmd str = write_stdout_to str cmd

  let ( >> ) cmd str = append_stdout_to str cmd

  let ( >! ) cmd str = write_stderr_to str cmd

  let ( >>! ) cmd str = append_stderr_to str cmd

  let ( < ) cmd str = read_stdin_from str cmd
end

let stdout_to_stderr cmd ~stdin_reader ~stdout_writer ~stderr_writer ~background
    ~cwd ~env =
  Unix.close stdout_writer;
  let stdout_writer = Unix.dup stderr_writer in
  cmd ~stdin_reader ~stdout_writer ~stderr_writer ~background ~cwd ~env

let stderr_to_stdout cmd ~stdin_reader ~stdout_writer ~stderr_writer:_
    ~background ~cwd ~env =
  (* Might cause a file descriptor leak, since we don't close stderr_writers.

     Not sure why, but trying to close the normal stderr_writer often results in chaos. *)
  let stderr_writer = Unix.dup stdout_writer in
  cmd ~stdin_reader ~stdout_writer ~stderr_writer ~background ~cwd ~env

(* === Misc === *)

let last_exit () = !State.exit

let devnull = "/dev/null"

let fzf ?cwd ?env cmd =
  let stdout = cmd |. process "fzf" [] |> collect_stdout ?cwd ?env in
  if last_exit () = 0 then Some stdout else None

(* === Signals === *)
let terminate_child_processes () =
  process "pgrep" [ "-P"; Pid.to_string State.pid ]
  |> collect_lines |> List.map ~f:Pid.of_string
  |> List.iter ~f:(fun pid ->
         ignore (Signal.send Signal.term (`Pid pid) : [ `Ok | `No_such_process ]))

let () = Caml.at_exit terminate_child_processes

(* === Tests === *)

let%test_module _ =
  ( module struct
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

    let%expect_test "redirection" =
      find "." ~ignore_hidden:true ~kind:`Files ~name:"*.ml"
      |. rg_v {|\.pp\.|} |> print;
      [%expect {|
        ./example.ml
        ./feather.ml |}]

    let%expect_test "redirection" =
      (* TODO: tests for redirection *)
      ()
  end )
