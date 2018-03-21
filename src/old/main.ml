open Lexing
open Parser
open Syntax
open Typesystem

let debug = ref false

let if_debug f =
    if !debug then
      f ()
    else
      ()

let input = ref ""

let string_of_pos (pos : position) : string =
  match pos with
  | { pos_fname = f; pos_lnum = l; pos_bol = b; pos_cnum = c } ->
     Printf.sprintf "File \"%s\", line %d, column %d" f l (c -b)

let main () =
  Arg.parse [
    ("--debug",
     Arg.Set debug,
     "Enable debug printing. (default = off)");
  ] (function s -> input := s) "usage: typecheck <input file>";

  let chan   = open_in !input in
  let lexbuf = Lexing.from_channel chan in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !input };
  let expr =
    (try
      Parser.start Lexer.token lexbuf
    with
    | Lexer.LexerError (p, msg) ->
       Printf.printf "%s\nError: %s" (string_of_pos p) msg;
       exit (-1))
  in
  if_debug (fun () -> print_endline (expr_to_string expr));
  let (t, env) = typecheck Scope.empty Constraints.empty Var.Map.empty Var.Map.empty expr in
  print_endline (Type.to_string t);
  print_endline (TEnv.to_string env)

;;

main ()
