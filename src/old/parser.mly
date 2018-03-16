%{
  open Syntax
  open Parsing
  open Type
%}

%start start
%token <Bit.t> TLBIT
%token <Kind.t> TKIND
%token <Label.t> TLABEL
%token <int> TLINT
%token <string> TIDENT

%token 
  TIF TTHEN TELSE
  TLET TAND TREC TEQ TBANG TBAND TCOLON TIN
  TCLPAR TRARROW TCOMMA TCRPAR
  TLPAR TAT TRPAR 
  TMUX TREVEAL TUSE TTOSS TEXISTS TTAND TINDEP
  TSLPAR TSRPAR TLARROW
  TARRAY TLENGTH TFUN
  TTYPE TTUNIT TBIT TINT TTFLIP TPLUS TSTAR TWILD
  TLSQ TRSQ TAS TDOT TBOTTOM TJOIN
  TEOF

%left TAT
%left TAND
%left TEQ
%right TRARROW


%type <Syntax.expr> start

%%
start : 
  expr TEOF { $1 }

;

expr :
  | TIDENT      { EVar (Var.Var $1) }
  | TLPAR TRPAR { EUnit }
  | TLBIT TLABEL { EBit ($1, $2) }
  | TLINT TLABEL { EInt (Int $1, $2) }
  | expr TPLUS expr { EPlus ($1, $3) }
  | expr TSTAR expr { EMult ($1, $3) }
  | expr TEQ expr { EEq ($1, $3) }
  | expr TBAND expr { EAnd ($1, $3) }
  | TBANG expr { ENot $2 }
  | TLPAR expr TCOMMA expr TRPAR { ETuple ($2, $4) }
  | TCLPAR expr_record_fields TCRPAR { ERecord $2 }
  | TARRAY TLPAR expr TRPAR TSLPAR TFUN TIDENT TRARROW expr TSRPAR { EArrayInit ($3, Var.Var $7, $9) }
  | expr TSLPAR expr TSRPAR { ERead ($1, $3) }
  | expr TSLPAR expr TSRPAR TLARROW expr { EReadWrite ($1, $3, $6) }
  | TLENGTH TLPAR expr TRPAR { ELen $3 }
  | TTOSS { EToss }
  | TUSE TLPAR TIDENT TRPAR { EUse (Var.Var $3) }
  | TREVEAL TLPAR TIDENT TRPAR { EReveal (Var.Var $3) }
  | TMUX TKIND TLPAR expr TCOMMA expr TCOMMA expr TRPAR { EMux ($2, $4, $6, $8) }
  | TTYPE TIDENT TEQ typ TIN expr { EAlias (Var.Var $2, $4, $6) }
  | TLET pat TEQ expr TIN expr { ELet ($2, $4, $6) }
  | TLET TREC pat TEQ expr mutual_recursion expr { let defs = ($3, $5) :: $6 in ELetRec (defs, $7) }
  | TLSQ regions TAS region_vars TDOT expr TCOLON typ TRSQ { EPack ($2, $4, $6, $8) }
  | expr TAT expr { EApp ($1, $3) }
  | TIF expr TTHEN expr TELSE expr { EIf ($2, $4, $6) }
  | TLPAR expr TRPAR { $2 }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected expression")) }

;

mutual_recursion :
  | TIN { [] }
  | TAND pat TEQ expr mutual_recursion { ($2, $4) :: $5 }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected mutually recursive definition")) }

;

expr_record_fields :
  | TIDENT TRARROW expr { [ (Field.Field $1, $3) ] }
  | expr_record_field expr_record_fields { $1 :: $2 }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected record fields")) }

;

expr_record_field :
  | TIDENT TRARROW expr TCOMMA { (Field.Field $1, $3) }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected a record field")) }

;


regions :
  | region { [$1] }
  | region TCOMMA regions { $1 :: $3 }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected regions")) }

;

region :
  | TBOTTOM { RExp.Bottom }
  | TIDENT { RExp.Var (Region.Region $1) }
  | region TJOIN region { RExp.Join ($1, $3) }
  | TLPAR region TRPAR { $2 }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected a region")) }

;

region_constraints:
  | { Constraints.empty }
  | region_constraint { Constraints.singleton $1 }
  | region_constraint TCOMMA region_constraints { Constraints.add $1 $3 }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected region constraints")) }

;

region_constraint:
  | region TINDEP region { Indep.Indep ($1, $3) }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected a region constraint")) }

;

pat :
  | TWILD { XWild } 
  | TIDENT { XVar (Var.Var $1) }
  | TLPAR TIDENT TCOMMA TIDENT TRPAR { XTuple (Var.Var $2, Var.Var $4) }
  | TLSQ region_vars TDOT TIDENT TRSQ { XUnpack ($2, Var.Var $4) }
  | TCLPAR pat_record_fields TCRPAR { XRecord $2 }
  | TIDENT pat_function_args { XFun (Var.Var $1, $2, None) }
  | TIDENT pat_function_args TCOLON typ { XFun (Var.Var $1, $2, Some $4) }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected a pattern")) }

;

region_vars :
  | TIDENT { [Region.Region $1] }
  | region_var region_vars { $1 :: $2 }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected region variables")) }

;

region_var :
  | TIDENT TCOMMA { Region.Region $1 }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected region variable")) }

;

pat_record_fields :
  | TIDENT TRARROW TIDENT { [ (Field.Field $1, Var.Var $3) ] }
  | pat_record_field pat_record_fields { $1 :: $2 }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected record pattern fields")) }

;

pat_record_field :
  | TIDENT TRARROW TIDENT TCOMMA { (Field.Field $1, Var.Var $3) }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected a record pattern field")) }

;

pat_function_args :
  | TLPAR TIDENT TCOLON typ TRPAR { [ (Var.Var $2, $4) ] }
  | pat_function_arg pat_function_args { $1 :: $2 }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected function pattern arguments")) }

;

pat_function_arg :
  | TLPAR TIDENT TCOLON typ TRPAR { (Var.Var $2, $4) }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected a function pattern argument")) }

;

typ :
  | TIDENT { TAlias (Var.Var $1) }
  | TTUNIT { TUnit }
  | TBIT TAT TLPAR TLABEL TCOMMA region TRPAR { TBit ($4, $6) }
  | TINT TAT TLPAR TLABEL TCOMMA region TRPAR { TInt ($4, $6) }
  | TTFLIP TAT TLPAR region TRPAR { TFlip $4 }
  | TEXISTS region_vars TDOT region_constraints TTAND typ { TExistential ($2, $4, $6) }
  | typ TSTAR typ { TTuple ($1, $3) }
  | typ TARRAY { TArray $1 }
  | typ TRARROW typ { TFun ($1, $3) }
  | TCLPAR typ_record_fields TCRPAR { TRecord $2 }
  | TLPAR typ TRPAR { $2 }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected a type")) }

;

typ_record_fields :
  | TIDENT TCOLON typ { Field.Map.singleton (Field.Field $1) $3 }
  | typ_record_field typ_record_fields { let (field, t) = $1 in Field.Map.add field t $2 }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected record type fields")) }

;

typ_record_field :
  | TIDENT TCOLON typ TCOMMA { (Field.Field $1, $3) }
  | error { raise (Error.SyntaxError (symbol_start_pos (), "expected record type field")) }
