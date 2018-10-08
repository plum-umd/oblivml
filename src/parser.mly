%{
  open Core
  open Stdio

  open Parsing

  open Expr

  exception SyntaxError of Position.t * String.t

  (** Annotate an AST node with positions in source. *)
  let annotate e =
    { loc  = Some ({ l_pos = symbol_start_pos ()
                   ; r_pos = symbol_end_pos ()
                   })
    ; node = e
    }

  let curry patterns body =
    List.fold_right patterns
                    ~init:body
                    ~f:(fun pat acc ->
                            annotate (EAbs { param = pat
                                           ; body  = acc
                                           }))

  let curry_rec name patterns body t_ret =
    match patterns with
    | []      -> failwith "Impossible: forbidden by lexer / parser."
    | p :: ps ->
      let (body_rec, t_rec) =
        List.fold_right
          ps
          ~init:(body, t_ret)
          ~f:(fun pat (body_acc, t_acc) ->
              let body_rec' = annotate (EAbs { param = pat
                                             ; body  = body_acc
                                             })
              in
              match pat with
              | Pattern.XAscr (_, t_param) ->
                let t_acc' = Type.TFun (t_param, t_acc) in
                (body_rec', t_acc')
              | _ -> raise (SyntaxError (symbol_start_pos (), "Recursive functions must have type annotated parameters.")))
      in
      annotate (ERec { name  = name
                     ; param = p
                     ; body  = body_rec
                     ; t_ret = t_rec
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

/** Random -> Non-Uniform */
%token               TTRUST

/** Non-Uniform -> Random */
%token               TPROVE

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

%left TLARROW TRARROW TIN TREGJOIN TELSE
%left TBAND
%left TLAND
%left TEQ
%left TPLUS TMINUS
%left TSTAR
%nonassoc TNOT TARRAY
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
  | TFUN patterns TDOT expr { curry $2 $4 }
  /** Recursive Abstraction */
  | TFUN TREC TVAR patterns TCOLON typ TDOT expr { curry_rec $3 $4 $8 $6 }
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
  | TLET TREC TVAR patterns TCOLON typ TEQ expr TIN expr { annotate (ELet { pat = Pattern.XVar $3
                                                                          ; value = curry_rec $3 $4 $8 $6
                                                                          ; body  = $10
                                                                          }) }
  /** Type Alias */
  | TTYPE TVAR TEQ typ TIN expr { annotate (EType { name = $2
                                                  ; typ   = $4
                                                  ; body  = $6
                                                  }) }
  /** Conditional */
  | TIF expr TTHEN expr TELSE expr { annotate (EIf { guard = $2
                                                    ; thenb = $4
                                                    ; elseb = $6
                                                    }) }
  /** Error */
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
  | TVAR path { annotate (EVar { path = $1 :: $2 }) }

  /** Record */
  | TCLPAR record_defs TCRPAR { annotate (ERecord { contents = $2
                                                 }) }
  /** Use */
  | TUSE TLPAR TVAR TRPAR { annotate (EUse { arg = $3
                                           }) }
  /** Reveal */
  | TREVEAL TLPAR TVAR TRPAR { annotate (EReveal { arg = $3
                                                 }) }
  /** Trust */
  | TTRUST TLPAR TVAR TRPAR { annotate (ETrust { arg = $3
                                               }) }
  /** Prove */
  | TPROVE TLPAR TVAR TRPAR { annotate (EProve { arg = $3
                                               }) }
  /** Grouping */
  | TLPAR expr TRPAR { $2 }
;

path :
  | { [] }
  | TDOT TVAR path { $2 :: $3 }
;


region :
  | TREGBOT { Region.bottom }
  | TREGVAR { Region.var $1 }
  | region TREGJOIN region { Region.join $1 $3 }
  /** Error */
  | error { raise (SyntaxError (symbol_start_pos (), "Expected a region.")) }
;

record_defs :
  | TVAR TEQ expr { [ ($1, $3) ] }
  | record_def record_defs { $1 :: $2 }
  /** Error */
  | error { raise (SyntaxError (symbol_start_pos (), "Expected a non-empty record.")) }
;

record_def :
  | TVAR TEQ expr TSEMI { ($1, $3) }
  /** Error */
  | error { raise (SyntaxError (symbol_start_pos (), "Expected a record definition.")) }
;

patterns :
  | pattern { [ $1 ] }
  | pattern patterns { $1 :: $2 }
  /** Error */
  | error { let p = symbol_start_pos () in Printf.printf "%s" (Position.to_string p); raise (SyntaxError (p, "Expected patterns.")) }
;

pattern :
  | TWILD { Pattern.XWild }
  | TVAR  { Pattern.XVar $1 }
  | TLPAR pattern TCOMMA pattern TRPAR { Pattern.XTuple ($2, $4) }
  | TCLPAR record_patterns TCRPAR { Pattern.XRecord $2 }
  | TLPAR pattern TCOLON typ TRPAR { Pattern.XAscr ($2, $4) }
;

record_patterns :
  | TVAR TEQ pattern { [ ($1, $3) ] }
  | record_pattern record_patterns { $1 :: $2 }
  /** Error */
  | error { raise (SyntaxError (symbol_start_pos (), "Expected record patterns.")) }
;

record_pattern :
  | TVAR TEQ pattern TSEMI { ($1, $3) }
  /** Error */
  | error { raise (SyntaxError (symbol_start_pos (), "Expected record pattern.")) }
;

typ :
  | TBTYP TALPAR TLABEL TCOMMA region TARPAR { Type.TBase ($1, $3, $5) }
  | TVAR { Type.TAlias $1 }
  | typ TSTAR typ { Type.TTuple ($1, $3) }
  | TCLPAR record_typs TCRPAR { Type.TRecord $2 }
  | typ TARRAY { Type.TArray $1 }
  | typ TRARROW typ { Type.TFun ($1, $3) }
  /** Error */
  | error { raise (SyntaxError (symbol_start_pos (), "Expected type.")) }
;

record_typs :
  | TVAR TCOLON typ { Map.singleton (module Var) $1 (Some $3) }
  | record_typ record_typs { let (x, t) = $1 in Map.set $2 ~key:x ~data:(Some t) }
  /** Error */
  | error { raise (SyntaxError (symbol_start_pos (), "Expected record types.")) }
;

record_typ :
  | TVAR TCOLON typ TSEMI { ($1, $3) }
  /** Error */
  | error { raise (SyntaxError (symbol_start_pos (), "Expected record type.")) }
;