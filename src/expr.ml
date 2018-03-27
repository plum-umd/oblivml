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
  | EFlip     of { label  : Label.t
                 ; region : Region.Expr.t
                 }

  (** Random Integer *)
  | ERnd      of { label  : Label.t
                 ; region : Region.Expr.t
                 }

  (** Variable *)
  | EVar      of { name : Var.t
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

  (** Unary Arithmetic Operation *)
  | EAUnOp    of { op  : Arith.Un.Op.t
                 ; arg : t
                 }

  (** Binary Arithmetic Operation *)
  | EABinOp   of { op  : Arith.Bin.Op.t
                 ; lhs : t
                 ; rhs : t
                 }

  (** Unary Arithmetic Relation *)
  | EAUnRel   of { rel  : Arith.Un.Rel.t
                 ; arg : t
                 }

  (** Binary Arithmetic Relation *)
  | EABinRel  of { rel : Arith.Bin.Rel.t
                 ; lhs : t
                 ; rhs : t
                 }

  (** Tuple *)
  | ETuple    of { contents : (t, t) Tuple.t
                 }

  (** Record *)
  | ERecord   of { contents : (Var.t * t) list
                 }

  (** Record Access *)
  | ERecAcc   of { record : t
                 ; field  : Var.t
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
  | EUse      of { arg : Var.t
                 }

  (** Random -> Public *)
  | EReveal   of { arg : Var.t
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
  | EType     of { alias : Var.t
                 ; typ   : Type.t
                 ; body  : t
                 }

  (** Conditional *)
  | EIf       of { guard : t
                 ; thenb : t
                 ; elseb : t
                 }
