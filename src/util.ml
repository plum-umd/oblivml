open Core
open Stdio

let parse_string s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "<string>" };
  try
    Some (Parser.start Lexer.token lexbuf)
  with
  | SyntaxError.SyntaxError (pos, msg) ->
    Printf.printf "%s\n%s" (Position.to_string pos) msg;
    None

let parse_file f   =
  let lexbuf = Lexing.from_channel (In_channel.create f) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f };
  try
    Some (Parser.start Lexer.token lexbuf)
  with
  | SyntaxError.SyntaxError (pos, msg) ->
    Printf.printf "%s\n%s" (Position.to_string pos) msg;
    None

let is_lo f = Filename.check_suffix f ".lo"

let parse_all dir =
  let fs = Sys.ls_dir dir in
  let fs =
    List.filter_map
      ~f:(fun f ->
          if is_lo f then
            Some (Filename.concat dir f)
          else
            None)
      fs
  in
  List.iter
    ~f:(fun f ->
        Printf.printf "Parsing %s...\n" f;
        let _ : Source.t Option.t = parse_file f in
        ())
    fs

let option_to_string t = function
  | None   -> "*"
  | Some x -> t x

let option_pp t f s = Format.pp_print_text f (option_to_string t s)

let option_pp_sect = option_pp Section.to_string
