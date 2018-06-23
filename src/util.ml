open Core
open Stdio

let parse_string s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "<string>" };
  Parser.start Lexer.token lexbuf

let parse_file f   =
  let lexbuf = Lexing.from_channel (In_channel.create f) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f };
  Parser.start Lexer.token lexbuf
