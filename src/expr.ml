open Core
open Stdio

type t =
  { loc  : Section.t Option.t
  ; node : t'
  }

and t' =
  | ELit      of { value  : Literal.t (** Literal *)
                 ; label  : Label.t
                 ; region : Region.t
                 }

  | EFlip     of { label  : Label.t   (** Random Boolean *)
                 ; region : Region.t
                 }

  | ERnd      of { label  : Label.t  (** Random Integer *)
                 ; region : Region.t
                 }

  | EVar      of { path : Var.t list  (** Variable *)
                 }

  | EBUnOp    of { op  : Boolean.Un.Op.t  (** Unary Boolean Operation *)
                 ; arg : t
                 }

  | EBBinOp   of { op  : Boolean.Bin.Op.t   (** Binary Boolean Operation *)
                 ; lhs : t
                 ; rhs : t
                 }

  | EAUnOp    of { op  : Arith.Un.Op.t  (** Unary Arithmetic Operation *)
                 ; arg : t
                 }

  | EABinOp   of { op  : Arith.Bin.Op.t   (** Binary Arithmetic Operation *)
                 ; lhs : t
                 ; rhs : t
                 }

  | EAUnRel   of { rel : Arith.Un.Rel.t   (** Unary Arithmetic Relation *)
                 ; arg : t
                 }

  | EABinRel  of { rel : Arith.Bin.Rel.t   (** Binary Arithmetic Relation *)
                 ; lhs : t
                 ; rhs : t
                 }

  | ETuple    of (t, t) Tuple.T2.t   (** Tuple *)

  | ERecord   of (Var.t * t) list   (** Record *)

  | EArrInit  of { size : t   (** Array Initialization *)
                 ; init : t
                 }

  | EArrRead  of { addr : t   (** Array Read *)
                 ; idx  : t
                 }


  | EArrWrite of { addr  : t   (** Array Write *)
                 ; idx   : t
                 ; value : t
                 }

  | EArrLen   of t   (** Array Length *)

  | EUse      of Var.t   (** Random -> Secret *)

  | EReveal   of Var.t   (** Random -> Public *)

  | ETrust    of Var.t   (** Random -> Non-Uniform *)

  | EProve    of Var.t   (** Non-Uniform -> Random *)

  | EMux      of { guard : t   (** Mux *)
                 ; lhs   : t
                 ; rhs   : t
                 }

  | EAbs      of { param : Pattern.t   (** Abstraction *)
                 ; body  : t
                 }

  | ERec      of { name  : Var.t   (** Recursive Abstraction *)
                 ; param : Pattern.t
                 ; body  : t
                 ; t_ret : Type.t
                 }

  | EApp      of { lam : t   (** Application *)
                 ; arg : t
                 }

  | ELet      of { pat   : Pattern.t   (** Let-Binding *)
                 ; value : t
                 ; body  : t
                 }

  | EType     of { name : Var.t   (** Type Alias *)
                 ; typ  : Type.t
                 ; body : t
                 }

  | EIf       of { guard : t   (** Conditional *)
                 ; thenb : t
                 ; elseb : t
                 }
  | EPrint    of t
