open! Base
open! Stdio
open Feather
open Feather.Infix

let __ () =
  cat "dune" |. rg "feather" |> stdout_to_stderr |. echo "hi" |. sort
  > "/tmp/out" |. uniq |. rg "h" |> run

let __ () =
  match ls "." < devnull |> fzf with
  | Some s -> printf "hi there: %s\n" s
  | None -> printf "got nothing\n"

let __ () = process "tail" [ "-n"; "2" ] < "dune-project" |. rg "w" |> run

let __ () =
  for i = 0 to 3 do
    let output = "/tmp/output" ^ Int.to_string i in
    cat "/usr/share/dict/words" |. shuf |. head 5 > output |> run;
    match tr "a-z" "A-Z" < output |> collect stdout |> lines with
    | [ a; b; c; d; e ] ->
        (printf "You are a %s %s and I think this is the %s %s of all %s.\n")
          a b c d e
    | _ -> failwith "head 5"
  done;

  ls "/tmp" |. grep "output" |> run

let __ () =
  let open Core in
  let module Time = Time_float_unix in
  echo "count hour" |> run;
  echo "----- ----" |> run;
  process "ls" [ "-lah" ] |. sed "  *" " " |. cut' [ 8 ]
  |. filter_lines ~f:(fun line -> String.( <> ) line "")
  |. map_lines ~f:(fun line ->
      Time.Ofday.of_string line |> Time.Ofday.to_span_since_start_of_day
      |> Time.Span.to_hr |> Float.to_int
      |> function
      | n when n > 12 -> Int.to_string (n - 12) ^ " PM"
      | n -> Int.to_string n ^ " AM")
  |. process "sort" [ "-n" ] |. process "uniq" [ "-c" ] |> run

let __ () =
  let stderr, status =
    ls "thisfiledoesnotexists" |> collect stderr_and_status
  in
  printf "ls returned error code %d: %s\n" status stderr

let __ () = ls "." |> run ~cwd:"_build"

let __ () =
  let stdout, stderr =
    (Feather.of_list [ "wow"; "hi" ] |> stdout_to_stderr) ->. echo "hi there"
    |> collect stdout_and_stderr
  in
  printf "*** %s ***\n" stderr;
  print_endline stdout

let __ () =
  Feather.of_list [ "duck"; "goose"; "duck" ]
  |. Feather.map_lines ~f:(fun s -> "*** " ^ s)
  |. sort |> collect stdout |> lines |> List.iter ~f:print_endline

let __ () =
  let process =
    process "bash" [ "-c"; "for i in `seq 1 5` ; do echo $i ; sleep 1 ; done" ]
    |> run_in_background
  in
  ignore process

let __ () =
  let p1 =
    process "bash"
      [ "-c"; "for i in `seq 1 5` ; do echo a $i ; sleep 1 ; done" ]
    |> run_in_background
  in
  let _ =
    Thread.create
      (fun () ->
        Feather.wait p1;
        print_endline "p1 done")
      ()
  in
  Unix.sleep 1;
  let p2 =
    process "bash"
      [ "-c"; "for i in `seq 6 10` ; do echo b $i ; sleep 1 ; done" ]
    |> run_in_background
  in
  let _ =
    Thread.create
      (fun () ->
        Feather.wait p2;
        print_endline "p2 done")
      ()
  in
  print_endline "main 98";
  Unix.sleep 2;
  print_endline "main 99";
  Feather.wait p1;
  Feather.wait p2;
  print_endline "main 100";
  Feather.wait p1

let __ () =
  for i = 1 to 10 do
    process "bash" [ "-c"; [%string "sleep %{i#Int} ; echo %{i#Int}"] ]
    |> run_in_background |> ignore
  done;
  Feather.wait_all ();
  print_endline "done"

let () =
  let p1 =
    process "echo" [ "test" ] |> collect_in_background stdout_and_status
  in
  print_endline "waiting now";
  let stdout, status = Feather.wait p1 in
  print_endline "==== stdout ====";
  print_endline stdout;
  print_endline "==== status ====";
  printf "%d\n" status
