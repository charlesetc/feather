# Feather

A minimal shell library for OCaml with lightweight, posix-like syntax.

For documentation and available commands, see [the generated
documentation](https://www.charlesetc.com/feather/feather/Feather/index.html)
or [feather.mli](./src/feather.mli).

## Basic Introduction

Feather exposes one type, `Feather.cmd`, along with a few building
blocks for creating, composing, and running commands:

You can create new commands from scratch with `Feather.process`:

``` ocaml
val process : string -> string list -> cmd
```

When passed to `Feather.run`, this will spawn a new unix process with
the given executable and arguments. For instance

``` ocaml
Feather.process "cat" [ "text.txt" ]
```

creates a `Feather.cmd` to cat `text.txt`. You can pipe commands
together with the `|.` operator, much like `|` in bash:

``` ocaml
Feather.process "cat" [ "text.txt" ] |. Feather.grep "wow"
```

How does one run a command? Well it depends on whether you want to use
the output. To run a command without capturing any output, simply use
`Feather.run`:

``` ocaml
val run : ?cwd:string -> ?env:(string * string) list -> cmd -> unit
```

This will block until the command finishes executing. You can also run a
command in the background with `Feather.run_in_background`. It will be killed
if it has not finished executing when the parent program terminates.

But if you want to use the process's stdout, stderr or status in OCaml, 
`Feather.collect` will be your friend:

``` ocaml
val collect : ?cwd:string -> ?env:(string * string) list -> 'a what_to_collect -> cmd -> 'a
```

The parameter of type `what_to_collect` can be `status`, `stdout`,
`stderr`, `stdout_and_stderr`, `stdout_and_status`, `stderr_and_status` or
`everything`, depending on what should be collected.

```ocaml
let stderr, status = command_1 |> collect stderr_and_status in
(* Feather.lines can be used to transform a long string into a list of its lines *)
let stdout_lines = command_2 |> collect stdout |> lines in ...
```

Perhaps the most important feature of Feather is that it lets you use
OCaml within a chain of pipes:

``` ocaml
utop# process "ps" [] |. map_lines String.uppercase |. grep "BASH" |> collect stdout;;
- : string = " 232699 PTS/4    00:00:00 BASH"
```

Feather also provides a bunch of wrappers around common unix commands
like `grep`, `find`, `sort`, etc. See
[feather.mli](./browse/feather.mli) for the full list.

Lastly, Feather has support for file descriptor redirection, and common 
shell chaining operators. Either with functions like

``` ocaml
val write_stderr_to : string -> cmd -> cmd

val append_stderr_to : string -> cmd -> cmd

val and_ : cmd -> cmd -> cmd

val or_ : cmd -> cmd -> cmd

val sequence : cmd -> cmd -> cmd
```

or with infix operators in `Feather.Infix`

``` ocaml
(* Stdout *)
val ( > ) : cmd -> string -> cmd

val ( >> ) : cmd -> string -> cmd

(* Stderr *)
val ( >! ) : cmd -> string -> cmd

val ( >>! ) : cmd -> string -> cmd

(* Executes second command if first is successful *)
val ( &&. ) : cmd -> cmd -> cmd

(* Executes second command if first fails *)
val ( ||. ) : cmd -> cmd -> cmd

(* Executes second command no matter what *)
val ( ->. ) : cmd -> cmd -> cmd
```

This does what you would expect:

``` ocaml
open Feather
open Feather.Infix

echo "hi"  > "/tmp/out"
```

That's pretty much Feather! Below are some examples in full.

## Examples

Say you wanted to make a quick sentence generator:

``` ocaml
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
```

With the result:

    $ make example
    You are a UNIVERSALIST SPARROWTONGUE and I think this is the TRICHOPHYTIA VARIATION of all ACCLIMATEMENT.
    You are a NONACCELERATION ERGOISM and I think this is the TENENT ONYMAL of all WINNONISH.
    You are a DOUGHMAKING UNSCHOLAR and I think this is the CROAKINESS HEARTSICKNESS of all EMOTE.
    You are a FLIRTER PRECURSOR and I think this is the POTOROUS INTERWEAVING of all INTERSEXUALITY.
    output0
    output1
    output2
    output3

Or perhaps group files in your home directory by what hour of the day
they were last modified:

<!-- TODO: Come up with a simpler example showcasing this -->

``` ocaml
let () =
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
```

With output: (The first column is how many files were last edited in
that hour timespan.)

    $ make example
    count hour
    ----- ----
       5 2 PM
       3 9 PM
       3 10 PM
       4 11 PM
       1 12 AM
       2 1 PM
       1 2 PM
       3 9 PM
       5 10 PM
       4 11 PM
       1 12 AM

## FAQ

### How do I install feather?

Feather is hosted on opam

    opam install feather

or you can build it from source

    git clone https://github.com/charlesetc/feather
    cd feather
    dune build

### Does feather work with Async or Lwt?

There is a [feather\_async](https://github.com/charlesetc/feather_async)
library that lets you use Feather in Async code! There is not an Lwt
equivalent yet, but the Async wrapper is very small so I would guess the
Lwt one would be too.

### How does this compare to other shell-scripting libraries, namely Shexp?

[Shexp](https://github.com/janestreet/shexp) is the main alternative
within the OCaml ecosystem. Shexp is more fully-featured, provides more
control over how processes launch, and is much better tested in general.

Feather differs in design from Shexp mainly because it favors a direct
style over a monadic one. In Shexp you incrementally construct a
`'a Shexp_process.t`, parametrized over the type you want. On the other
hand, a `Feather.cmd` is not parametrized: you run it to get a string
which can be parsed directly by OCaml later. No monads in sight!

As a comparison, say you wanted to count the number of characters from
"ls" using Shexp:

```ocaml
let (number_of_chars : int Shexp_process.t) =
  let%map.Shexp_process _, stdout =
    Shexp_process.run "ls" [] |> Shexp_process.capture [ Stdout ]
  in
  String.length stdout
in
let length = Shexp_process.eval number_of_chars in
print_int length
```

or another way using `Shexp_process.Infix`:

``` ocaml
let ( let+ ) x f = Shexp_process.bind x ~f

let () =
  let open Shexp_process.Infix in
  eval
    (Shexp_process.call [ "ls" ]
    |- let+ s = Shexp_process.read_all in
       print_int (String.length s))
```

...and here is the equivalent Feather:

``` ocaml
let length =
  Feather.process "ls" [] |> Feather.collect stdout |> String.length
in
print_int length
```
