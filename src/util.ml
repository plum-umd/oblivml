let parse_string s = Parser.start Lexer.token (Lexing.from_string s)

let parse_file f = Parser.start Lexer.token (Lexing.from_channel (open_in f))
