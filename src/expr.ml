open Base

type t =
  { loc  : Section.t Option.t
  ; node : t'
  }

and t' =
  (** Literal *)
  | ELit      of { value  : Literal.t
                 ; label  : Label.t
                 ; region : Region.Expr.t
                 }

  (** Random Boolean *)
  | EFlip

  (** Random Integer *)
  | ERnd

  (** Variable *)
  | EVar      of { name : Var.t
                 }

  (** Unary Arithmetic Operation *)
  | EAUnOp    of { op  : Arith.Un.Op.t
                 ; arg : t
                 }

  (** Binary Arithmetic Operation *)
  | EABinOp   of { op  : Arith.Bin.Op.t
                 ; lhs : t
                 ; rhs : t
                 }

  (** Unary Relational Operation *)
  | EAUnRel   of { op  : Arith.Un.Rel.t
                 ; arg : t
                 }

  (** Binary Relation Operation *)
  | EABinRel  of { rel : Arith.Bin.Rel.t
                 ; lhs : t
                 ; rhs : t
                 }

  (** Unary Boolean Operation *)
  | EBUnOp    of { op  : Boolean.Un.Op.t
                 ; arg : t
                 }

  (** Binary Boolean Operation *)
  | EBBinOp   of { op  : Boolean.Bin.Op.t
                 ; lhs : t
                 ; rhs : t
                 }

  (** Tuple *)
  | ETuple    of { contents : (t, t) Tuple.t
                 }

  (** Record *)
  | ERecord   of { contents : (Field.t, t) Record.t
                 }

  (** Array Initialization *)
  | EArrInit  of { size : t
                 ; init : t
                 }

  (** Array Read *)
  | EArrRead  of { addr : t
                 ; idx  : t
                 }

  (** Array Write *)
  | EArrWrite of { addr  : t
                 ; idx   : t
                 ; value : t
                 }

  (** Array Length *)
  | EArrLen   of { addr : t
                 }

  (** Random -> Secret *)
  | EUse      of { arg : t
                 }

  (** Random -> Public *)
  | EReveal   of { arg : t
                 }

  (** Mux *)
  | EMux      of { guard : t
                 ; lhs   : t
                 ; rhs   : t
                 }

  (** Abstraction *)
  | EAbs      of { param : Pattern.t
                 ; body  : t
                 }

  (** Recursive Abstraction *)
  | ERec      of { name  : Var.t
                 ; param : Pattern.t
                 ; body  : t
                 }

  (** Application *)
  | EApp      of { lam : t
                 ; arg : t
                 }

  (** Let-Binding *)
  | ELet      of { pat   : Pattern.t
                 ; value : t
                 ; body  : t
                 }

  (** Type Alias *)
  | EType     of { alias : Type.Alias.t
                 ; typ   : Type.t
                 }

  (** Conditional *)
  | EIf       of { guard : t
                 ; thenb : t
                 ; elseb : t
                 }
