open Core

type config = { env   : (Var.t, Expr.value, Var.comparator_witness) Map.t
              ; store : (Loc.t, Expr.value, Loc.comparator_witness) Map.t
              ; expr : Expr.t }

type trace = config List.t

module Dynamic (Dist : Monad.Basic) =
struct
  module Syntax = Monad.Make(Dist)

  let step (c : config) : (config Dist.t) Option.t =
    match c.expr.node with
    | Expr.ELit l -> None
    | _ -> None

  let rec eval' (dt : trace Dist.t) (c : config) : trace Dist.t =
    let open Syntax.Let_syntax in
    let%bind t = dt in
    let dt' = Dist.return (c :: t) in
    match step c with
    | None     -> dt'
    | Some dc' ->
      let%bind c' = dc' in
      eval' dt' c'

  let eval e = eval' (Dist.return []) { env = Map.empty (module Var) ; store = Map.empty (module Loc) ; expr = e }
end
