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
  echo "count hour" |> run;
  echo "----- ----" |> run;
  process "ls" [ "-lah" ]
  |. sed "  *" " "
  |. cut' [ 8 ]
  |. filter_lines ~f:(fun line -> String.( <> ) line "")
  |. map_lines ~f:(fun line ->
         Time.Ofday.of_string line |> Time.Ofday.to_span_since_start_of_day
         |> Time.Span.to_hr |> Float.to_int
         |> function
         | n when n > 12 -> Int.to_string (n - 12) ^ " PM"
         | n -> Int.to_string n ^ " AM")
  |. process "sort" [ "-n" ]
  |. process "uniq" [ "-c" ]
  |> run

let __ () =
  let stderr, status =
    ls "thisfiledoesnotexists" |> collect stderr_and_status
  in
  printf "ls returned error code %d: %s\n" status stderr

let __ () = ls "." |> run ~cwd:"_build"

let () =
  let stdout, stderr =
    (Feather.of_list [ "wow"; "hi" ] |> stdout_to_stderr) ->. echo "hi there"
    |> collect stdout_and_stderr
  in
  printf "*** %s ***\n" stderr;
  print_endline stdout
