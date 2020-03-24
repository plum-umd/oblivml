open Core
open Stdio

exception SyntaxError of Position.t * String.t

type t = { source_location  : Section.t
         ; datum : t'
         }

and t' =
  | ELit      of { datum  : Literal.t (** Literal *)
                 ; label  : Label.t
                 }

  | EFlip     of Region.t (** Random Boolean *)

  | ERnd      of Region.t (** Random Integer *)

  | EVar      of Var.t List.t (** Variable *)

  | EBOp    of { op  : Boolean.Op.t (** Boolean Operation *)
               ; args : t List.t
               }

  | EAOp    of { op  : Arith.Op.t (** Arithmetic Operation *)
               ; args : t List.t
               }

  | EARel   of { rel : Arith.Rel.t (** Arithmetic Relation *)
                 ; args : t List.t
                 }

  | ETuple    of (t, t) Tuple.T2.t   (** Tuple *)

  | ERecord   of (Var.t * t) List.t   (** Record *)

  | EArrInit  of { size : t   (** Array Initialization *)
                 ; init : t
                 }

  | EArrRead  of { loc : t   (** Array Read *)
                 ; idx  : t
                 }


  | EArrWrite of { loc  : t   (** Array Write *)
                 ; idx   : t
                 ; value : t
                 }

  | EArrLen   of t   (** Array Length *)

  | ECast     of { var : Var.t ; label : Label.t }

  | EMux      of { guard : t   (** Mux *)
                 ; lhs   : t
                 ; rhs   : t
                 }

  | EAbs      of { pat : Pattern.t   (** Abstraction *)
                 ; body  : t
                 }

  | ERec      of { name  : Var.t   (** Recursive Abstraction *)
                 ; pat : Pattern.t
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
