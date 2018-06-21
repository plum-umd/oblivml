{
  open Core
  open Stdio

  open Lexing
  open Parser

  exception SyntaxError of Position.t * String.t
}

let whitespace = [' ' '\t' ]
let newline    = ('\n' | '\r' '\n')

let letter     = ['a'-'z' 'A'-'Z']
let digit      = ['0'-'9']

(** Bool Literal *)
let bool_l     = ("true" | "false")

(** Int Literal *)
let int_l      = '-'? digit+

(** Label *)
let label      = ("public" | "secret")

(** Region *)
let rbottom    = "_|_"
let rvar       = '`' letter (letter | digit | [''' '_'])*
let rjoin      = "\\/"

(** Types *)
let base_t     = ("unit" | "bool" | "int" | "rbool" | "rint")

(** Variable *)
let var        = letter (letter | digit | [''' '_'])*

(**********************************
 **** OblivML Lexical Analysis ****
 **********************************)

rule token = parse
  (** Whitespace *)
  | whitespace { token lexbuf }

  (** Newline *)
  | newline    { new_line lexbuf; token lexbuf }

  (** Unit Literal *)
  | "()"       { TLIT (Literal.LitUnit ()) }

  (** Bool Literal *)
  | bool_l     { TLIT (Literal.LitBool (Bool.of_string (lexeme lexbuf))) }

  (** Int Literal *)
  | int_l      { TLIT (Literal.LitInt (Int.of_string (lexeme lexbuf))) }

  (** Label *)
  | label      { TLABEL (Label.of_string (lexeme lexbuf)) }

  (** Region *)
  | rbottom    { TREGBOT }
  | rvar       { TREGVAR (Var.Var (lexeme lexbuf)) }
  | rjoin      { TREGJOIN }

  (** Flip *)
  | "flip"     { TFLIP }

  (** Rnd *)
  | "rnd"      { TRND }

  (** Unary Boolean Operation *)
  | "not"      { TNOT }

  (** Binary Boolean Operation *)
  | "&&"       { TBAND }

  (** Unary Arithmetic Operation *)
  (* N/A -- Placeholder *)

  (** Binary Arithmetic Operation *)
  | "+"        { TPLUS }
  | "-"        { TMINUS }
  | "*"        { TSTAR }
  | "&"        { TLAND }

  (** Unary Arithmetic Relation *)
  (* N/A -- Placeholder *)

  (** Binary Arithmetic Relation *)
  | "="        { TEQ }

  (** Tuple *)
  | "("        { TLPAR }
  | ","        { TCOMMA }
  | ")"        { TRPAR }

  (** Record *)
  | "{"        { TCLPAR }
  | ";"        { TSEMI }
  | "}"        { TCRPAR }

  (** Record Access *)
  | "."        { TDOT }

  (** Array Initialization *)
  | "array"    { TARRAY }
  | "["        { TSLPAR }
  | "]"        { TSRPAR }

  (** Array Read *)
  (* N/A -- Covered by `Array Initialization` *)

  (** Array Write *)
  | "<-"       { TLARROW }

  (** Array Length *)
  | "length"   { TLENGTH }

  (** Random -> Secret (Use) *)
  | "use"      { TUSE }

  (** Random -> Public (Reveal) *)
  | "reveal"   { TREVEAL }

  (** Mux *)
  | "mux"      { TMUX }

  (** Abstraction *)
  | "fun"      { TFUN }
  | "->"       { TRARROW }

  (** Pattern *)
  | "_"        { TWILD }

  (** Recursive Abstraction *)
  (* N/A -- Covered by `Abstraction` and `Record Access` *)

  (** Application *)
  (* N/A -- Application is juxtaposition *)

  (** Binding *)
  | "let"      { TLET }
  | "rec"      { TREC }
  | "in"       { TIN }

  (** Type Alias *)
  | "type"     { TTYPE }

  (** Type *)
  | base_t     { TBTYP (Type.Base.of_string (lexeme lexbuf)) }
  | ":"        { TCOLON }
  | "<"        { TALPAR }
  | ">"        { TARPAR }

  (** Conditional *)
  | "if"       { TIF }
  | "then"     { TTHEN }
  | "else"     { TELSE }

  (** Identifier *)
  | var        { TVAR (Var.Var (lexeme lexbuf)) }

  (** Comment *)
  | "(*"       { comment (lexeme_start_p lexbuf) lexbuf; token lexbuf }
  | "*)"       { raise (SyntaxError (lexeme_start_p lexbuf, "This comment terminator has no corresponding initiator.")) }

  (** EOF *)
  | eof        { TEOF }

  (** Failure *)
  | _           { raise (SyntaxError (lexeme_start_p lexbuf, "Unexpected token.")) }

and comment pos_inner = parse
  | "(*"    { comment (lexeme_start_p lexbuf) lexbuf; comment pos_inner lexbuf }
  | "*)"    { () }
  | eof     { raise (SyntaxError (pos_inner, "This comment initiator has no corresponding terminator.")) }
  | newline { new_line lexbuf; comment pos_inner lexbuf }
