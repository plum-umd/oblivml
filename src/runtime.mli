open Core

(** Runtime expressions with source location information.
    Parameterized by value type. *)
type 'v t = { source_location : Section.t
            ; datum : 'v t' }

and 'v t' =
  | ELit      of { datum : Literal.t ; label : Label.t }
  | EVal      of 'v
  | EFlip     of Region.t
  | ERnd      of Region.t
  | EVar      of Var.t List.t
  | EBOp      of { op : Boolean.Op.t ; args : 'v t List.t }
  | EAOp      of { op : Arith.Op.t ; args : 'v t List.t }
  | EARel     of { rel : Arith.Rel.t ; args : 'v t List.t }
  | ETuple    of ('v t, 'v t) Tuple.T2.t
  | ERecord   of (Var.t * 'v t) List.t
  | EArrInit  of { size : 'v t ; init : 'v t }
  | EArrRead  of { loc : 'v t ; idx : 'v t }
  | EArrWrite of { loc : 'v t ; idx : 'v t ; value : 'v t }
  | EArrLen   of 'v t
  | ECast     of { var : Var.t ; label : Label.t }
  | EMux      of { guard : 'v t ; lhs : 'v t ; rhs : 'v t }
  | EAbs      of { pat : Pattern.t ; body : 'v t }
  | ERec      of { name : Var.t ; pat : Pattern.t ; body : 'v t }
  | EApp      of { lam : 'v t ; arg : 'v t }
  | ELet      of { pat : Pattern.t ; value : 'v t ; body : 'v t }
  | EIf       of { guard : 'v t ; thenb : 'v t ; elseb : 'v t }
  | EPrint    of 'v t

(** [is_value e] returns [true] if [e] is a value. *)
val is_value : 'v t -> Bool.t

(** [is_redex e] returns [true] if [e] is a reducible expression. *)
val is_redex : 'v t -> Bool.t

(** [of_source s] returns the runtime expression corresponding to [s]. *)
val of_source : Source.t -> 'v t

(** [of_value v] returns the embedding of [v] into a runtime expression. *)
val of_value : 'v Value.t -> 'v t

(** [of_values vs] maps [of_value v] over [vs]. *)
val of_values : 'v Value.t List.t -> 'v t List.t

(** [to_value e] returns the value [v] which is embedded in [e]. *)
val to_value : 'v t -> 'v Value.t

(** [to_values es] maps [to_value e] over [es]. *)
val to_values : 'v t List.t -> 'v Value.t List.t
