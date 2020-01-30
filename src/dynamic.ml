open Core

type config = { n : Int.t
              ; env   : (Var.t, Expr.value, Var.comparator_witness) Map.t
              ; store : (Loc.t, Expr.value Array.t, Loc.comparator_witness) Map.t
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
  | Expr.EVar p -> Some (IDist.return { c with expr = { c.expr with node = Expr.EVal (lookup c.env p.path) } })
  | Expr.EBUnOp buo ->
    Some
      begin
        match buo.arg.node with
        | Expr.EVal v ->
          (match v.value with
           | VBool b ->
             (match buo.op with
              | Boolean.Un.Op.Not -> IDist.return { c with expr = { c.expr with node = Expr.EVal { v with value = VBool (IDist.negate b) } } })
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
             (match v1.value, v2.value with
              | VBool b1, VBool b2 ->
                (match bbo.op with
                 | Boolean.Bin.Op.And -> IDist.return { c with expr = { c.expr with node = Expr.EVal { value = VBool (IDist.bitand b1 b2) ; label = Label.join v1.label v2.label } } })
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
  | Expr.ETuple (lhs, rhs) ->
    Some
      begin
        match lhs.node with
        | Expr.EVal v1 ->
          (match rhs.node with
           | Expr.EVal v2 ->
             IDist.return { c with expr = { c.expr with node = Expr.EVal { value = VTuple (v1, v2) ; label = Label.meet v1.label v2.label } } }
           | _ ->
             let open Syntax.Let_syntax in
             (match step { c with expr = rhs } with
              | None -> failwith "Impossible by typing"
              | Some dc' ->
                let%bind c' = dc' in
                IDist.return { c' with expr = { c.expr with node = Expr.ETuple (lhs, c'.expr) } }))
        | _ ->
          let open Syntax.Let_syntax in
          (match step { c with expr = lhs } with
           | None -> failwith "Impossible by typing"
           | Some dc' ->
             let%bind c' = dc' in
             IDist.return { c' with expr = { c.expr with node = Expr.ETuple (c'.expr, rhs) } })
      end
  | Expr.ERecord fields -> failwith "TODO"
  | Expr.EArrInit spec ->
    Some
      begin
        match spec.size.node with
        | Expr.EVal vsize ->
          (match spec.init.node with
           | Expr.EVal vinit ->
             (match vsize.value with
              | VInt dn ->
                let open Syntax.Let_syntax in
                let%bind n = dn in
                let l = Loc.fresh () in
                IDist.return { c with expr = { c.expr with node = Expr.EVal { value = VLoc l ; label = Label.public } }
                                    ; store = Map.add_exn c.store ~key:l ~data:(Array.init n ~f:(fun _ -> vinit)) }
              | _ -> failwith "Impossible by typing")
           | _ ->
             let open Syntax.Let_syntax in
             (match step { c with expr = spec.init } with
              | None -> failwith "Impossible by typing"
              | Some dc' ->
                let%bind c' = dc' in
                IDist.return { c' with expr = { c.expr with node = Expr.EArrInit { spec with init = c'.expr } } }))
        | _ ->
          let open Syntax.Let_syntax in
          (match step { c with expr = spec.size } with
           | None -> failwith "Impossible by typing"
           | Some dc' ->
             let%bind c' = dc' in
             IDist.return { c' with expr = { c.expr with node = Expr.EArrInit { spec with size = c'.expr } } })
      end
  | Expr.EArrRead rd ->
    Some
      begin
        match rd.addr.node with
        | Expr.EVal vaddr ->
          (match rd.idx.node with
           | Expr.EVal vidx ->
             (match vaddr.value with
              | VLoc l ->
                let storev = Map.find_exn c.store l in
                (match vidx.value with
                 | VInt dn ->
                   let open Syntax.Let_syntax in
                   let%bind n = dn in
                   IDist.return { c with expr = { c.expr with node = Expr.EVal storev.(n) } }
                 | _ -> failwith "Impossible by typing")
              | _ -> failwith "Impossible by typing")
           | _ ->
             let open Syntax.Let_syntax in
             (match step { c with expr = rd.idx } with
              | None -> failwith "Impossible by typing"
              | Some dc' ->
                let%bind c' = dc' in
                IDist.return { c' with expr = { c.expr with node = Expr.EArrRead { rd with idx = c'.expr } } }))
        | _ ->
          let open Syntax.Let_syntax in
          (match step { c with expr = rd.addr } with
           | None -> failwith "Impossible by typing"
           | Some dc' ->
             let%bind c' = dc' in
             IDist.return { c' with expr = { c.expr with node = Expr.EArrRead { rd with addr = c'.expr } } })
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
