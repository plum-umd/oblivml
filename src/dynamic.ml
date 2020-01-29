open Core

type config = { n : Int.t
              ; env   : (Var.t, Expr.value, Var.comparator_witness) Map.t
              ; store : (Loc.t, Expr.value, Loc.comparator_witness) Map.t
              ; expr : Expr.t }

type trace = config List.t

module Syntax = Monad.Make(IDist)

let rec lookup e p : Expr.value =
  match p with
  | [] -> failwith "Impossible"
  | var :: path ->
    List.fold_left
      ~init:(Map.find_exn e var)
      ~f:(fun (acc : Expr.value) x ->
          match acc.value with
          | Expr.VRecord fields -> let m = Map.of_alist_exn (module Var) fields in Map.find_exn m x
          | _ -> failwith "Not a record")
      p

let rec step (c : config) : (config IDist.t) Option.t =
  match c.expr.node with
  | Expr.ELit l ->
    (match l.value with
     | LitUnit () -> Some (IDist.return { c with expr = { c.expr with node = Expr.EVal { contents = { value = VUnit (IDist.return ()) ; label = l.label } } } })
     | LitBool b  -> Some (IDist.return { c with expr = { c.expr with node = Expr.EVal { contents = { value = VBool (IDist.return b) ; label = l.label } } } })
     | LitInt  n  -> Some (IDist.return { c with expr = { c.expr with node = Expr.EVal { contents = { value = VInt (IDist.return n) ; label = l.label } } } }))
  | Expr.EVal _ -> None
  | Expr.EFlip fl -> Some (IDist.return { c with n = c.n + 1 ; expr = { c.expr with node = Expr.EVal { contents = { value = VFlip (IDist.bit c.n) ; label = fl.label } } } })
  | Expr.ERnd r ->
    let b1 = IDist.bit c.n in
    let b2 = IDist.bit (c.n + 1) in
    let b3 = IDist.bit (c.n + 2) in
    let b4 = IDist.bit (c.n + 3) in
    Some (IDist.return { c with n = c.n + 4 ; expr = { c.expr with node = Expr.EVal { contents = { value = VRnd ([b1 ; b2 ; b3 ; b4]) ; label = r.label } } } })
  | Expr.EVar p -> Some (IDist.return { c with expr = { c.expr with node = Expr.EVal { contents = (lookup c.env p.path) } } })
  | Expr.EBUnOp buo ->
    Some
      begin
        match buo.arg.node with
        | Expr.EVal v ->
          (match v.contents.value with
           | VBool b ->
             (match buo.op with
              | Boolean.Un.Op.Not -> IDist.return { c with expr = { c.expr with node = Expr.EVal { contents = { v.contents with value = VBool (IDist.negate b) } } } })
           | _ -> failwith "Impossible by typing")
        | _ ->
          let open Syntax.Let_syntax in
          (match step { c with expr = buo.arg } with
           | None -> failwith "Impossible by typing"
           | Some dc' ->
             let%bind c' = dc' in
             IDist.return { c' with expr = { c.expr with node = Expr.EBUnOp { buo with arg = c'.expr } } })
      end
  | Expr.EBBinOp bbo ->
    Some
      begin
        match bbo.lhs.node with
        | Expr.EVal v1 ->
          (match bbo.rhs.node with
           | Expr.EVal v2 ->
             (match v1.contents.value, v2.contents.value with
              | VBool b1, VBool b2 ->
                (match bbo.op with
                 | Boolean.Bin.Op.And -> IDist.return { c with expr = { c.expr with node = Expr.EVal { contents = { value = VBool (IDist.bitand b1 b2) ; label = Label.join v1.contents.label v2.contents.label } } } })
              | _ -> failwith "Impossible by typing")
           | _ ->
             let open Syntax.Let_syntax in
             (match step { c with expr = bbo.rhs } with
              | None -> failwith "Impossible by typing"
              | Some dc' ->
                let%bind c' = dc' in
                IDist.return { c' with expr = { c.expr with node = Expr.EBBinOp { bbo with rhs = c'.expr } } }))
        | _ ->
          let open Syntax.Let_syntax in
          (match step { c with expr = bbo.lhs } with
           | None -> failwith "Impossible by typing"
           | Some dc' ->
             let%bind c' = dc' in
             IDist.return { c' with expr = { c.expr with node = Expr.EBBinOp { bbo with lhs = c'.expr } } })
      end
  | Expr.EAUnOp _ -> failwith "TODO"
  | Expr.EABinOp _ -> failwith "TODO"
  | Expr.EAUnRel _ -> failwith "TODO"
  | Expr.EABinRel _ -> failwith "TODO"
  | Expr.ETuple tup ->
    let (lhs, rhs) = tup.contents in
    Some
      begin
        match lhs.node with
        | Expr.EVal v1 ->
          (match rhs.node with
           | Expr.EVal v2 ->
             IDist.return { c with expr = { c.expr with node = Expr.EVal { contents = { value = VTuple (v1.contents, v2.contents) ; label = Label.meet v1.contents.label v2.contents.label } } } }
           | _ ->
             let open Syntax.Let_syntax in
             (match step { c with expr = rhs } with
              | None -> failwith "Impossible by typing"
              | Some dc' ->
                let%bind c' = dc' in
                IDist.return { c' with expr = { c.expr with node = Expr.ETuple { contents = (lhs, c'.expr) } } }))
        | _ ->
          let open Syntax.Let_syntax in
          (match step { c with expr = lhs } with
           | None -> failwith "Impossible by typing"
           | Some dc' ->
             let%bind c' = dc' in
             IDist.return { c' with expr = { c.expr with node = Expr.ETuple { contents = (c'.expr, rhs) } } })
      end
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
