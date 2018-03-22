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

%left TREGJOIN
%left TBAND
%left TLAND
%left TEQ
%left TPLUS TMINUS
%left TSTAR
%nonassoc TNOT
%left TDOT

/*************************
 **** OBLIVML Parsing ****
 *************************/

%%
start :
  | expr TEOF { $1 }
;

expr :
  /** Literal */
  | TLIT TALPAR TLABEL TCOMMA region TARPAR { annotate (ELit { value  = $1
                                                             ; label  = $3
                                                             ; region = $5
                                                             }) }
  /** Flip */
  | TFLIP TALPAR TLABEL TCOMMA region TARPAR { annotate (EFlip { label  = $3
                                                               ; region = $5
                                                               }) }
  /** Rnd */
  | TRND TALPAR TLABEL TCOMMA region TARPAR { annotate (ERnd { label  = $3
                                                             ; region = $5
                                                             }) }
  /** Var */
  | TVAR { annotate (EVar { name = $1
                          }) }
  /** Unary Boolean Operation */
  | TNOT expr { annotate (EBUnOp { op  = Boolean.Un.Op.Not
                                 ; arg = $2
                                 }) }
  /** Binary Boolean Operation */
  | expr TBAND expr { annotate (EBBinOp { op  = Boolean.Bin.Op.And
                                        ; lhs = $1
                                        ; rhs = $3
                                        }) }
  /** Binary Arithmetic Operation */
  | expr TPLUS expr { annotate (EABinOp { op  = Arith.Bin.Op.Add
                                        ; lhs = $1
                                        ; rhs = $3
                                        }) }
  | expr TMINUS expr { annotate (EABinOp { op  = Arith.Bin.Op.Subtract
                                         ; lhs = $1
                                         ; rhs = $3
                                         }) }
  | expr TSTAR expr { annotate (EABinOp { op  = Arith.Bin.Op.Mult
                                        ; lhs = $1
                                        ; rhs = $3
                                        }) }
  | expr TLAND expr { annotate (EABinOp { op  = Arith.Bin.Op.And
                                        ; lhs = $1
                                        ; rhs = $3
                                        }) }
  /** Binary Arithmetic Relation */
  | expr TEQ expr { annotate (EABinRel { rel = Arith.Bin.Rel.Equal
                                       ; lhs = $1
                                       ; rhs = $3
                                       }) }
  /** Tuple */
  | TLPAR expr TCOMMA expr TRPAR { annotate (ETuple { contents = ($2, $4)
                                                    }) }
  /** Record */
  | TCLPAR record_defs TCRPAR { annotate (ERecord { contents = $2
                                                 }) }
  | expr TDOT TVAR { annotate (ERecAcc { record = $1
                                       ; field  = $3
                                       }) }
  /** Array */
  | TARRAY TLPAR expr TRPAR TSLPAR expr TSRPAR { annotate (EArrInit { size = $3
                                                                    ; init = $6
                                                                    }) }
  | expr TSLPAR expr TSRPAR { annotate (EArrRead { addr = $1
                                                 ; idx  = $3
                                                 }) }
  | error { raise (SyntaxError (symbol_start_pos (), "Expected an expression.")) }
;

region :
  | TREGBOT { Region.Expr.Bot }
  | TREGVAR { Region.Expr.Var $1 }
  | region TREGJOIN region { Region.Expr.Join ($1, $3) }
;

record_defs :
  | TVAR TEQ expr { [ ($1, $3) ] }
  | record_def record_defs { $1 :: $2 }
;

record_def :
  | TVAR TEQ expr TSEMI { ($1, $3) }