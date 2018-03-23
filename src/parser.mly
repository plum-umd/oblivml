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

  let curry patterns body =
    List.fold_right (fun pat acc -> annotate (EAbs { param = pat
                                                   ; body  = acc
                                                   })) patterns body

  let curry_rec name patterns body =
    match patterns with
    | []      -> raise Impossible (* forbidden by parser *)
    | p :: ps -> annotate (ERec { name  = name
                                ; param = p
                                ; body  = List.fold_right
                                            (fun pat acc -> annotate (EAbs { param = pat
                                                                           ; body  = acc
                                                                           }))
                                            ps
                                            body
                                })
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

/** Pattern */
%token               TWILD

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

%left TLARROW TRARROW TIN TREGJOIN
%left TBAND
%left TLAND
%left TEQ
%left TPLUS TMINUS
%left TSTAR
%nonassoc TNOT
%left TSLPAR TSRPAR TDOT

/*************************
 **** OBLIVML Parsing ****
 *************************/

%%
start :
  | expr TEOF { $1 }
;

expr :
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
  /** Record Access */
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
  | expr TSLPAR expr TSRPAR TLARROW expr { annotate (EArrWrite { addr  = $1
                                                               ; idx   = $3
                                                               ; value = $6
                                                               }) }
  | TLENGTH TLPAR expr TRPAR { annotate (EArrLen { addr = $3
                                                 }) }
  /** Mux */
  | TMUX TLPAR expr TCOMMA expr TCOMMA expr TRPAR { annotate (EMux { guard = $3
                                                                   ; lhs   = $5
                                                                   ; rhs   = $7
                                                                   }) }
  /** Abstraction */
  | TFUN patterns TRARROW expr { curry $2 $4 }
  /** Recursive Abstraction */
  | TFUN TVAR TDOT patterns TRARROW expr { curry_rec $2 $4 $6 }
  /** Application (Stage) */
  | fexpr { $1 }
  /** Binding */
  | TLET pattern TEQ expr TIN expr { annotate (ELet { pat   = $2
                                                    ; value = $4
                                                    ; body  = $6
                                                    }) }
  | TLET TVAR patterns TEQ expr TIN expr { annotate (ELet { pat   = Pattern.XVar $2
                                                          ; value = curry $3 $5
                                                          ; body  = $7
                                                          }) }
  | TLET TREC TVAR patterns TEQ expr TIN expr { annotate (ELet { pat   = Pattern.XVar $3
                                                               ; value = curry_rec $3 $4 $6
                                                               ; body  = $8
                                                               }) }
  /** Type Alias */
  | error { raise (SyntaxError (symbol_start_pos (), "Expected an expression.")) }
;

fexpr :
  | atexpr { $1 }
  /** Application */
  | fexpr atexpr { annotate (EApp { lam = $1
                                  ; arg = $2
                                  }) }
;

atexpr :
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
  /** Record */
  | TCLPAR record_defs TCRPAR { annotate (ERecord { contents = $2
                                                 }) }
  /** Use */
  | TUSE TLPAR TVAR TRPAR { annotate (EUse { arg = $3
                                           }) }
  /** Reveal */
  | TREVEAL TLPAR TVAR TRPAR { annotate (EReveal { arg = $3
                                                 }) }
  /** Grouping */
  | TLPAR expr TRPAR { $2 }
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
;

patterns :
  | pattern { [ $1 ] }
  | pattern patterns { $1 :: $2 }
;

pattern :
  | TWILD { Pattern.XWild }
  | TVAR  { Pattern.XVar $1 }
  | TLPAR pattern TCOMMA pattern TRPAR { Pattern.XTuple ($2, $4) }
  | TCLPAR record_patterns TCRPAR { Pattern.XRecord $2 }
;

record_patterns :
  | TVAR TEQ pattern { [ ($1, $3) ] }
  | record_pattern record_patterns { $1 :: $2 }
;

record_pattern :
  | TVAR TEQ pattern TSEMI { ($1, $3) }