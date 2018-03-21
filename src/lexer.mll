{
  open Lexing
  open Parser
  open Syntax
  open Error

  let extract_intconst s = int_of_string s

  let extract_boolconst s =
    match s with
      | "true"  -> true
      | "false" -> false
      | _       -> raise Impossible (* forbidden by lexer *)

  let extract_label s =
    match s with
      | "public" -> Label.public
      | "secret" -> Label.secret
      | _        -> raise Impossible (* forbidden by lexer *)

  let extract_type s =
    match s with
      | "int"   -> Type.TBInt
      | "bool"  -> Type.TBBool
      | "rint"  -> Type.TBRInt
      | "rbool" -> Type.TBRBool
      | "unit"  -> Type.TBUnit
      | _       -> raise Impossible (* forbidden by lexer *)

  let extract_kind s =
    match s with
      | "affine"    -> Kind.Affine
      | "universal" -> Kind.Universal
      | _           -> raise Impossible (* forbidden by lexer *)
}

let whitespace = [' ' '\t' ]
let newline    = ('\n' | '\r' '\n')

let alpha      = ['a'-'z' 'A'-'Z']
let digit      = ['0'-'9']

(** Value Identifiers --
      Any sequences of letters, decimal digits,
      primes ('), or underbars (_) starting with a letter *)
let vid        = alpha (alpha | digit | [''' '_'])*

(** Integer Constants --
      An optional negation symbol (-), followed by
      a non-empty sequence of decimal digits. *)
let intconst   = '-'? digit+

(** Boolean Constants --
      Either the literal 'true' or 'false'. *)
let boolconst  = ("true" | "false")

(** Labels --
      Either the literal 'public' or 'secret'. *)
let label      = ("public" | "secret")

(** Kinds --
      Either the literal 'affine' or 'universal'. *)
let kind       = ("affine" | "universal")

(** Base Types --
      The literals 'int', 'bool', 'rint', 'rbool', or 'unit'. *)
let type       = ("int" | "bool" | "rint" | "rbool" | "unit")

(** Tokenizing OblivML *)
rule token = parse
(** Whitespace -- ignored *)
  | whitespace  { token lexbuf }                    (* we ignore whitespace *)

(** Newlines -- update location, then ignored *)
  | newline     { new_line lexbuf; token lexbuf }   (* we update lexer and ignore the newline *)

(** Variable Binding *)
  | "let"       { TLET }
  | "rec"       { TREC }
  | "in"        { TIN }
  | "="         { TEQ }
  | "and"       { TAND }

(** Conditionals *)
  | "if"        { TIF }
  | "then"      { TTHEN }
  | "else"      { TELSE }

(** Muxes *)
  | "mux"       { TMUX }

(** Random to Base *)
  | "use"       { TUSE }
  | "reveal"    { TREVEAL }

(** Integer Operators *)
  | "+"         { TPLUS }
  | "*"         { TSTAR }

(** Boolean Operators *)
  | "&&"        { TBAND }
  | "||"        { TBOR }
  | "not"       { TBNOT }

(** Bitwise Operators *)
  | "&"         { TTAND }
  | "|"         { TTOR }
  | "~"         { TTNOT }

(** Records *)
  | "{"         { TCLPAR }
  | ","         { TCOMMA }
  | "}"         { TCRPAR }

(** Functions *)
  | "fun"       { TFUN }
  | "->"        { TRARROW }

(** Arrays *)
  | "array"     { TARRAY }
  | "["         { TSLPAR }
  | "]"         { TSRPAR }
  | "<-"        { TLARROW }
  | "length"    { TLENGTH }

(** Type Ascription *)
  | "<"         { TALPAR }
  | ">"         { TARPAR }
  | ":"         { TCOLON }
  | "("         { TLPAR }
  | ")"         { TRPAR }

(** Patterns *)
  | "_"         { TWILD }

(** Integer Constants *)
  | intconst    { TLINT (extract_intconst (lexeme lexbuf)) }

(** Boolean Constants *)
  | boolconst   { TLBOOL (extract_boolconst (lexeme lexbuf)) }

(** Labels *)
  | label       { TLABEL (extract_label (lexeme lexbuf)) }

(** Base Types *)
  | type        { TTYPE (extract_type (lexeme lexbuf)) }

(** Kinds *)
  | kind        { TKIND (extract_kind (lexeme lexbuf)) }

(** Random Integers *)
  | "rnd"       { TRND }

(** Random Boolean *)
  | "flip"      { TFLIP }

(** Value Identifiers *)
  | vid         { TIDENT (lexeme lexbuf) }

(** Comments *)
  | "(*"        { comment (lexeme_start_p lexbuf) lexbuf; token lexbuf }
  | "*)"        { raise (SyntaxError (lexeme_start_p lexbuf, "This comment terminator has no corresponding initiator.")) }

(** EOF *)
  | eof         { TEOF }

(** Failure -- couldn't find a valid token *)
  | _           { raise (SyntaxError (lexeme_start_p lexbuf, "Unexpected token.")) }

and comment pos_inner = parse
  | "(*"    { comment (lexeme_start_p lexbuf) lexbuf; comment pos_inner lexbuf }
  | "*)"    { () }
  | eof     { raise (Error.SyntaxError (pos_inner, "This comment initiator has no corresponding terminator.")) }
  | newline { new_line lexbuf; comment pos_inner lexbuf }
  | _       { comment pos_inner lexbuf }
