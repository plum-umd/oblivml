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

(** Variable Binding

+ Basic: `let p1 = e2 in e3`
+ Recursive: `let rec p1 = e2 in e3`
+ Mutually Recursive: `let rec p1 = e2 and p3 = e4 in e5`
*)
  | "let"       { TLET }
  | "rec"       { TREC }
  | "in"        { TIN }
  | "="         { TEQ }
  | "and"       { TAND }

(** Conditionals

+ Basic: `if e1 then e2 else e3`
*)
  | "if"        { TIF }
  | "then"      { TTHEN }
  | "else"      { TELSE }

(** Muxes

+ Basic: `mux (e1, e1, e2)`
*)
  | "mux"       { TMUX }
  | "("         { TLPAR }
  | ")"         { TRPAR }

(** Convert Random Value

+ Secretly: `use(x)`
+ Publicly: `reveal(x)`
*)
  | "use"       { TUSE }
  | "reveal"    { TREVEAL }

(** Integer Constants *)
  | intconst    { TLIT (Literal.LitInt (extract_intconst (lexeme lexbuf))) }

(** Integer Operators

+ Addition: `e1 + e2`
+ Subtractiont: `e1 - e2`
+ Multiplication: `e1 * e2`
*)
  | "+"         { TPLUS }
  | "-"         { TMINUS }
  | "*"         { TSTAR }

(** Boolean Operators

+ Conjunction: `e1 && e2`
+ Disjunction: `e1 || e2`
+ Negation: `not e`
*)
  | "&&"        { TBAND }
  | "||"        { TBOR }
  | "not"       { TBNOT }

(** Bitwise Operators

+ AND: `e1 & e1`
+ OR: `e1 | e2`
+ NOT: `~ e`
*)
  | "&"         { TTAND }
  | "|"         { TTOR }
  | "~"         { TTNOT }

(** Records

+ Literals: `{ l1 = e1; l2 = e2; l3 = e3 }`
+ Patterns: `{ l1 = p1; l2 = p2; l3 = p3 }`
+ Access: `r1.f1`
*)
  | "{"         { TCLPAR }
  | ";"         { TSEMI }
  | "}"         { TCRPAR }
  | "."         { TDOT }

(** Functions

+ Literals: `fun p1 p2 -> e`
(* TODO(ins): recursive literals? `fun foo . p1 p2 -> e` *)
*)
  | "fun"       { TFUN }
  | "->"        { TRARROW }

(** Arrays

+ Creation: `array(e1)[fun x -> e2]`
+ Read: `e1[e2]`
+ Write: `e1[e2] <- e3`
+ Length: `length(e)`
*)
  | "array"     { TARRAY }
  | "["         { TSLPAR }
  | "]"         { TSRPAR }
  | "<-"        { TLARROW }
  | "length"    { TLENGTH }

(** Type Ascription

+ Basic: `(e1 : t1)`
*)
  | ":"         { TCOLON }

(** Type Construction

+ Base Types: `bt<l1, r1>`
*)
  | "<"         { TALPAR }
  | ","         { TCOMMA }
  | ">"         { TARPAR }

(** Patterns

+ Wildcard: `_`
*)
  | "_"         { TWILD }

(** Type Aliasing

+ Basic: `type x = t`
*)
  | "type"      { TTYPE }

(** Boolean Constants *)
  | boolconst   { TLBOOL (extract_boolconst (lexeme lexbuf)) }

(** Labels *)
  | label       { TLABEL (extract_label (lexeme lexbuf)) }

(** Regions *)
  | region      { TREGION (extract_region (lexeme lexbuf)) }

(** Base Types *)
  | type        { TBTYPE (extract_type (lexeme lexbuf)) }

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
  | eof     { raise (SyntaxError (pos_inner, "This comment initiator has no corresponding terminator.")) }
  | newline { new_line lexbuf; comment pos_inner lexbuf }
  | _       { comment pos_inner lexbuf }
