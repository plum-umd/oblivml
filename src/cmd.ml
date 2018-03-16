(** Stores the filename for input files. *)
let input = ref None

(** Usage string *)
let usage =
  "\nusage: oblivml [file]\n\n" ^
  "If `file` is a single caret (`^'), oblivml reads from the standard input.\n" ^
  "If no `file` is provided, oblivml will launch a REPL.\n"

(** Command-line flag definition *)
let debug = ref false

let flags =
  [ ("--debug",
     Arg.Set debug,
     "Enable debug printing. (default = off)") ]


(** Utilities for querying configuration *)

let if_debug f =
  if !debug then
    f ()
  else
    ()

(** Setup the application *)
let setup () =
  Arg.parse flags (fun s -> input := (Some s)) usage
