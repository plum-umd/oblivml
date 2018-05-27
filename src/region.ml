open Core
open Stdio

module Expr =
  struct
    type t =
      | Bot
      | Var of Var.t
      | Join of t * t

    let rec to_string rexpr =
      match rexpr with
      | Bot   -> "_|_"
      | Var x -> Var.to_string x
      | Join (rexpr1, rexpr2) -> Printf.sprintf "%s \\/ %s" (to_string rexpr1) (to_string rexpr2)

    let eq rexpr1 rexpr2 = rexpr1 = rexpr2
  end
