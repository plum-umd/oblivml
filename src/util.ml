open Core
open Stdio

let parse_string s = Parser.start Lexer.token (Lexing.from_string s)

let parse_file f = Parser.start Lexer.token (Lexing.from_channel (In_channel.create f))
