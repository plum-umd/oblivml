open Core
open Stdio

exception SyntaxError of Position.t * String.t

let parse_string s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "<string>" };
  try
    Some (Parser.start Lexer.token lexbuf)
  with
  | SyntaxError (pos, msg) ->
    Printf.printf "%s\n%s" (Position.to_string pos) msg;
    None

let parse_file f   =
  let lexbuf = Lexing.from_channel (In_channel.create f) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f };
  try
    Some (Parser.start Lexer.token lexbuf)
  with
  | SyntaxError (pos, msg) ->
    Printf.printf "%s\n%s" (Position.to_string pos) msg;
    None

let option_to_string t = function
  | None   -> "*"
  | Some x -> t x

let option_pp t f s = Format.pp_print_text f (option_to_string t s)

let option_pp_sect = option_pp Section.to_string
