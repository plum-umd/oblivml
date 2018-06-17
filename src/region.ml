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

    let normalize rexpr =
      match rexpr with
      | Join (Bot, r2) -> r2
      | Join (r1, Bot) -> r1
      | _ -> rexpr

    let equiv rexpr1 rexpr2 = eq (normalize rexpr1) (normalize rexpr2)
  end
