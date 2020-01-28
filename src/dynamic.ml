open Core

type config = { n : Int.t
              ; env   : (Var.t, Expr.value, Var.comparator_witness) Map.t
              ; store : (Loc.t, Expr.value, Loc.comparator_witness) Map.t
              ; expr : Expr.t }

type trace = config List.t

module Syntax = Monad.Make(IDist)

let lookup e p : Expr.value = failwith "TODO"

let rec step (c : config) : (config IDist.t) Option.t =
  match c.expr.node with
  | Expr.ELit l ->
    (match l.value with
     | LitUnit () -> Some (IDist.return { c with expr = { c.expr with node = Expr.EVal { value = VUnit (IDist.return ()) ; label = l.label } } })
     | LitBool b  -> Some (IDist.return { c with expr = { c.expr with node = Expr.EVal { value = VBool (IDist.return b) ; label = l.label } } })
     | LitInt  n  -> Some (IDist.return { c with expr = { c.expr with node = Expr.EVal { value = VInt (IDist.return n) ; label = l.label } } }))
  | Expr.EVal _ -> None
  | Expr.EFlip fl -> Some (IDist.return { c with n = c.n + 1 ; expr = { c.expr with node = Expr.EVal { value = VFlip (IDist.bit c.n) ; label = fl.label } } })
  | Expr.ERnd r ->
    let b1 = IDist.bit c.n in
    let b2 = IDist.bit (c.n + 1) in
    let b3 = IDist.bit (c.n + 2) in
    let b4 = IDist.bit (c.n + 3) in
    Some (IDist.return { c with n = c.n + 4 ; expr = { c.expr with node = Expr.EVal { value = VRnd ([b1 ; b2 ; b3 ; b4]) ; label = r.label } } })
  | Expr.EVar p -> Some (IDist.return { c with expr = { c.expr with node = Expr.EVal { value = (lookup c.env p.path) ; label = failwith "sigh" } } })
  | Expr.EBUnOp buo ->
    (match buo.arg.node with
     | Expr.EVal v ->
       (match v.value with
        | VBool b ->
          (match buo.op with
           | Boolean.Un.Op.Not -> Some (IDist.return { c with expr = { c.expr with node = Expr.EVal { v with value = VBool (IDist.negate b) } } }))
        | _ -> failwith "Impossible by typing")
     | _ ->
       Some
         begin
           let open Syntax.Let_syntax in
           (match step { c with expr = buo.arg } with
           | None -> failwith "Impossible by typing"
           | Some dc' ->
             let%bind c' = dc' in
             IDist.return { c' with expr = { c.expr with node = Expr.EBUnOp { buo with arg = c'.expr } } })
         end)
  | _ -> failwith "TODO"

let rec eval' (dt : trace IDist.t) (c : config) : trace IDist.t =
  let open Syntax.Let_syntax in
  let%bind t = dt in
  let dt' = IDist.return (c :: t) in
  match step c with
  | None     -> dt'
  | Some dc' ->
    let%bind c' = dc' in
    eval' dt' c'

let eval e = eval' (IDist.return []) { n = 0 ; env = Map.empty (module Var) ; store = Map.empty (module Loc) ; expr = e }
