%{
  open Parsing
  open Expr
  open Error

  (** Annotate an AST node with positions in source. *)
  let annotate e =
    { loc  = Some ({ l_pos = symbol_start_pos ()
                   ; r_pos = symbol_end_pos ()
                   })
    ; node = e
    }

%}

/***************************
 **** TOKEN DEFINITIONS ****
 ***************************/

/** Literal */
%token <Literal.t>   TLIT

/** Label */
%token <Label.t>     TLABEL

/** Region */
%token               TREGBOT
%token <Var.t>       TREGVAR
%token               TREGJOIN

/** Flip */
%token               TFLIP

/** Rnd */
%token               TRND

/** Unary Boolean Operation */
%token               TNOT

/** Binary Boolean Operation */
%token               TBAND

/** Unary Arithmetic Operation */
/* N/A -- Placeholder */

/** Binary Arithmetic Operation */
%token               TPLUS
%token               TMINUS
%token               TSTAR
%token               TLAND

/** Unary Arithmetic Relation */
/* N/A -- Placeholder */

/** Binary Arithmetic Relation */
%token               TEQ

/** Tuple */
%token               TLPAR
%token               TCOMMA
%token               TRPAR

/** Record */
%token               TCLPAR
%token               TSEMI
%token               TCRPAR

/** Record Access */
%token               TDOT

/** Array Initialization */
%token               TARRAY
%token               TSLPAR
%token               TSRPAR

/** Array Read */
/* N/A -- Covered by `Array Initialization` */

/** Array Write */
%token               TLARROW

/** Array Length */
%token               TLENGTH

/** Random -> Secret (Use) */
%token               TUSE

/** Random -> Public (Reveal) */
%token               TREVEAL

/** Mux */
%token               TMUX

/** Abstraction */
%token               TFUN
%token               TRARROW

/** Recursive Abstraction */
/* N/A -- Covered by `Abstraction` and `Record Access` */

/** Application */
/* N/A -- Application is juxtaposition */

/** Binding */
%token               TLET
%token               TREC
%token               TIN

/** Type Alias */
%token               TTYPE

/** Type */
%token <Type.Base.t> TBTYP
%token               TCOLON
%token               TALPAR
%token               TARPAR

/** Conditional */
%token               TIF
%token               TTHEN
%token               TELSE

/** Identifier */
%token <Var.t>       TVAR

/** EOF */
%token               TEOF

/*********************************
 **** PARSER ENTRY PRODUCTION ****
 *********************************/

%start start
%type <Expr.t> start

/*************************
 **** OBLIVML Parsing ****
 *************************/

%%
start :
  | expr TEOF { $1 }
;

expr :
  | TLIT TALPAR TLABEL TCOMMA region TARPAR { annotate (ELit { value  = $1
                                                             ; label  = $3
                                                             ; region = $5
                                                             }) }
  | error { raise (SyntaxError (symbol_start_pos (), "Expected an expression.")) }
;

region :
  | TREGBOT { Region.Expr.Bot }
  | TREGVAR { Region.Expr.Var $1 }
  | region TREGJOIN region { Region.Expr.Join ($1, $3) }
