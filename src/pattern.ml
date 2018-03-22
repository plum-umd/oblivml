type t =
  | XWild
  | XVar of Var.t
  | XTuple of t * t
  | XRecord of (Var.t * t) list
