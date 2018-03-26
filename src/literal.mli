open Base

type t =
  | LitUnit of Unit.t
  | LitBool of Bool.t
  | LitInt  of Int.t

val to_type : t -> Type.Base.t
