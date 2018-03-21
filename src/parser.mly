%{
  open Parsing
  open Syntax
  open Error

  let annotate e =
    { loc  = Some ({ sect_start = symbol_start_pos ()
                   ; sect_end   = symbol_end_pos ()
                   })
    ; node = e
    }

%}

%start start

%token <int>       TLINT
%token <bool>      TLBOOL
%token <Label.t>   TLABEL
%token <Type.base> TBTYPE
%token <Kind.t>    TKIND
%token <string>    TIDENT

%token
  TLET TREC TIN TEQ TAND
  TIF TTHEN TELSE
  TMUX TLPAR TRPAR
  TUSE TREVEAL
  TPLUS TMINUS TSTAR
  TBAND TBOR TBNOT
  TTAND TTOR TTNOT
  TCLPAR TSEMI TCRPAR TDOT
  TFUN TRARROW
  TARRAY TSLPAR TSRPAR TLARROW TLENGTH
  TCOLON
  TALPAR TCOMMA TARPAR
  TWILD
  TTYPE
  TRND
  TFLIP
  TEOF

%type <Syntax.oblivml> start

%%
start :
  | oblivml TEOF { $1 }
;

oblivml :
  | TLPAR TRPAR   { annotate EUnit }
  | TLABEL TLBOOL { annotate (EBool { value = $2
                                    ; label = $1
                                    }) }
  | TIDENT        { annotate (EVar  { name  = Var.Var $1
                                    }) }
  | error { raise (SyntaxError (symbol_start_pos (), "Expected an expression.")) }