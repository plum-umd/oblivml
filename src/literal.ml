open Base

type t =
  | LitUnit of Unit.t
  | LitBool of Bool.t
  | LitInt  of Int.t
