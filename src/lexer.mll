{
  open Lexing
  open Parser
  open Syntax

  let bit_arg s =
    match s with
      | "O" -> Bit.O
      | "I" -> Bit.I
      | _   -> failwith "Impossible"

  let kind_arg s =
    match s with
      | "universal" -> Kind.U
      | "affine"    -> Kind.A
      | _           -> failwith "Impossible"

  let label_arg s =
    match s with
      | "public" -> Label.bottom
      | "secret" -> Label.BaseSet.singleton (Label.Label 1)
      | _        -> failwith "Impossible"
}

(* Some handy regexps *)

let whitespace = [' ' '\t' ]
let newline    = ('\n' | '\r' '\n')
let linecomment = '/' '/' [^'\n']*

let alpha      = ['a'-'z' 'A'-'Z']
let num        = '-'? ['0'-'9']+
let endident   = (alpha | num | ['_' '$' '''])*
let ident      = alpha+ endident

let bit        = ('O' | 'I')
let kind       = ("universal" | "affine")
let labeled_typ = ("bit" | "int")
let label      = ("public" | "secret")


(* Token generators and rules *)

rule token = parse
| whitespace  { token lexbuf }                    (* we ignore whitespace *)
| newline     { new_line lexbuf; token lexbuf }   (* we update lexer and ignore the newline *)
| "if"        { TIF }
| "then"      { TTHEN }
| "else"      { TELSE }
| "let"       { TLET }
| "and"       { TAND }
| "rec"       { TREC }
| "="         { TEQ }
| ":"         { TCOLON }
| "!"         { TBANG }
| "&&"        { TBAND }
| "&"         { TTAND }
| "in"        { TIN }
| "{"         { TCLPAR }
| "->"        { TRARROW }
| ","         { TCOMMA }
| "."         { TDOT }
| "}"         { TCRPAR }
| "("         { TLPAR }
| ")"         { TRPAR }
| "mux"       { TMUX }
| "reveal"    { TREVEAL }
| "use"       { TUSE }
| "toss"      { TTOSS }
| "["         { TSLPAR }
| "]"         { TSRPAR }
| "<-"        { TLARROW }
| "array"     { TARRAY }
| "exists"    { TEXISTS }
| "length"    { TLENGTH }
| "fun"       { TFUN }
| "type"      { TTYPE }
| "*"         { TSTAR }
| "+"         { TPLUS }
| "@"         { TAT }
| "_"         { TWILD }
| bit         { TLBIT (bit_arg (lexeme lexbuf)) }
| kind        { TKIND (kind_arg (lexeme lexbuf)) }
| "unit"      { TTUNIT }
| "bit"       { TBIT }
| "int"       { TINT }
| "flip"      { TTFLIP }
| label       { TLABEL (label_arg (lexeme lexbuf)) }
| num         { TLINT (int_of_string (lexeme lexbuf)) }
| "<"         { TLSQ }
| ">"         { TRSQ }
| "as"        { TAS }
| "_|_"       { TBOTTOM }
| "_||_"      { TINDEP }
| "\\/"       { TJOIN }
| ident       { TIDENT (lexeme lexbuf) }
| eof         { TEOF }
| "(*"        { comment (lexeme_start_p lexbuf) lexbuf; token lexbuf }
| "*)"        { raise (Error.SyntaxError (lexeme_start_p lexbuf, "Comment not started")) }

and comment pos_inner = parse
    | "(*"    { comment (lexeme_start_p lexbuf) lexbuf; comment pos_inner lexbuf }
    | "*)"    { () }
    | eof     { raise (Error.SyntaxError (pos_inner, "Comment not terminated")) }
    | newline { new_line lexbuf; comment pos_inner lexbuf }
    | _       { comment pos_inner lexbuf }
