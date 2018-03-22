module Expr =
  struct
    type t =
      | Bot
      | Var of Var.t
      | Join of t * t

  end
