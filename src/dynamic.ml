open Core

type config = { n     : Int.t
              ; env   : (Var.t, Mixed.value, Var.comparator_witness) Map.t
              ; store : (Loc.t, Mixed.value Array.t, Loc.comparator_witness) Map.t
              ; expr  : Mixed.t }

type trace = config List.t

module Syntax = Monad.Make(IDist)

let rec of_source (e : Expr.t) : Mixed.t =
  match e.node with
  | ELit l -> ELit { value = l.value ; label = l.label }
  | EFlip _ -> EFlip
  | ERnd _ -> ERnd
  | EVar p -> EVar { path = p.path }
  | EBUnOp b -> EBUnOp { op = b.op ; arg = of_source b.arg }
  | EBBinOp b -> EBBinOp { op = b.op ; lhs = of_source b.lhs ; rhs = of_source b.rhs }
  | EAUnOp a -> EAUnOp { op = a.op ; arg = of_source a.arg }
  | EABinOp a -> EABinOp { op = a.op ; lhs = of_source a.lhs ; rhs = of_source a.rhs }
  | EAUnRel a -> EAUnRel { rel = a.rel ; arg = of_source a.arg }
  | EABinRel a -> EABinRel { rel = a.rel ; lhs = of_source a.lhs ; rhs = of_source a.rhs }
  | ETuple (l, r) -> ETuple (of_source l, of_source r)
  | ERecord fields -> ERecord (List.map ~f:(fun (x, e) -> (x, of_source e)) fields)
  | EArrInit sp -> EArrInit { size = of_source sp.size ; init = of_source sp.init }
  | EArrRead sp -> EArrRead { addr = of_source sp.addr ; idx = of_source sp.idx }
  | EArrWrite sp -> EArrWrite { addr = of_source sp.addr ; idx = of_source sp.idx ; value = of_source sp.value }
  | EArrLen len -> EArrLen (of_source len)
  | EUse x -> EUse x
  | EReveal x -> EReveal x
  | ETrust x -> ETrust x
  | EProve x -> EProve x
  | EMux m -> EMux { guard = of_source m.guard ; lhs = of_source m.lhs ; rhs = of_source m.rhs }
  | EAbs f -> EAbs { param = f.param ; body = of_source f.body }
  | ERec f -> ERec { name = f.name ; param = f.param ; body = of_source f.body }
  | EApp ap -> EApp { lam = of_source ap.lam ; arg = of_source ap.arg }
  | ELet l -> ELet { pat = l.pat ; value = of_source l.value ; body = of_source l.body }
  | EType t -> of_source t.body
  | EIf ite -> EIf { guard = of_source ite.guard ; thenb = of_source ite.thenb ; elseb = of_source ite.elseb }

let rec lookup e p : Mixed.value =
  match p with
  | [] -> failwith "Impossible"
  | var :: path ->
    List.fold_left
      ~init:(Map.find_exn e var)
      ~f:(fun acc x ->
          match acc with
          | Mixed.VRecord fields -> let m = Map.of_alist_exn (module Var) fields in Map.find_exn m x
          | _ -> failwith "Not a record")
      p

let rec step (c : config) : (config IDist.t) Option.t =
  match c.expr with
  | Mixed.EVal _ -> None
  | _ ->
    Some
      begin
        match c.expr with
        | Mixed.ELit l ->
          (match l.value with
           | LitUnit () -> IDist.return { c with expr = Mixed.EVal (VUnit (IDist.return ())) }
           | LitBool b  -> IDist.return { c with expr = Mixed.EVal (VBool { value = (IDist.return b) ; label = l.label }) }
           | LitInt  n  -> IDist.return { c with expr = Mixed.EVal (VInt  { value = (IDist.return n) ; label = l.label }) })
        | Mixed.EFlip -> IDist.return { c with n = c.n + 1 ; expr = Mixed.EVal (VFlip (IDist.bit c.n)) }
        | Mixed.ERnd ->
          (* TODO(ins): Should parameterize this better *)
          let bitwidth = 4 in
          let word = List.init bitwidth ~f:(fun idx -> IDist.bit (c.n + idx)) in
          IDist.return { c with n = c.n + bitwidth ; expr = Mixed.EVal (VRnd word) }
        | Mixed.EVar p -> IDist.return { c with expr = Mixed.EVal (lookup c.env p.path) }
        | Mixed.EBUnOp buo ->
          (match buo.arg with
           | Mixed.EVal v ->
             (match v with
              | VBool b ->
                (match buo.op with
                 | Boolean.Un.Op.Not -> IDist.return { c with expr = Mixed.EVal (VBool { b with value = (IDist.negate b.value) }) })
              | _ -> failwith "Impossible by typing")
           | _ ->
             let open Syntax.Let_syntax in
             (match step { c with expr = buo.arg } with
              | None -> failwith "Impossible by typing"
              | Some dc' ->
                let%bind c' = dc' in
                IDist.return { c' with expr = Mixed.EBUnOp { buo with arg = c'.expr } }))
        | Mixed.EBBinOp bbo ->
          (match bbo.lhs with
           | Mixed.EVal v1 ->
             (match bbo.rhs with
              | Mixed.EVal v2 ->
                (match v1, v2 with
                 | VBool b1, VBool b2 ->
                   (match bbo.op with
                    | Boolean.Bin.Op.And -> IDist.return { c with expr = Mixed.EVal (VBool { value = (IDist.bitand b1.value b2.value) ; label = Label.join b1.label b2.label }) })
                 | _ -> failwith "Impossible by typing")
              | _ ->
                let open Syntax.Let_syntax in
                (match step { c with expr = bbo.rhs } with
                 | None -> failwith "Impossible by typing"
                 | Some dc' ->
                   let%bind c' = dc' in
                   IDist.return { c' with expr = Mixed.EBBinOp { bbo with rhs = c'.expr } }))
           | _ ->
             let open Syntax.Let_syntax in
             (match step { c with expr = bbo.lhs } with
              | None -> failwith "Impossible by typing"
              | Some dc' ->
                let%bind c' = dc' in
                IDist.return { c' with expr = Mixed.EBBinOp { bbo with lhs = c'.expr } }))
        | Mixed.EAUnOp _ -> failwith "TODO"
        | Mixed.EABinOp _ -> failwith "TODO"
        | Mixed.EAUnRel _ -> failwith "TODO"
        | Mixed.EABinRel _ -> failwith "TODO"
        | Mixed.ETuple (lhs, rhs) ->
          (match lhs with
           | Mixed.EVal v1 ->
             (match rhs with
              | Mixed.EVal v2 ->
                IDist.return { c with expr = Mixed.EVal (VTuple (v1, v2)) }
              | _ ->
                let open Syntax.Let_syntax in
                (match step { c with expr = rhs } with
                 | None -> failwith "Impossible by typing"
                 | Some dc' ->
                   let%bind c' = dc' in
                   IDist.return { c' with expr = Mixed.ETuple (lhs, c'.expr) }))
           | _ ->
             let open Syntax.Let_syntax in
             (match step { c with expr = lhs } with
              | None -> failwith "Impossible by typing"
              | Some dc' ->
                let%bind c' = dc' in
                IDist.return { c' with expr = Mixed.ETuple (c'.expr, rhs) }))
        | Mixed.ERecord fields -> failwith "TODO"
        | Mixed.EArrInit spec ->
          (match spec.size with
           | Mixed.EVal vsize ->
             (match spec.init with
              | Mixed.EVal vinit ->
                (match vsize with
                 | VInt dn ->
                   let open Syntax.Let_syntax in
                   let%bind n = dn.value in
                   let l = Loc.fresh () in
                   IDist.return { c with expr = Mixed.EVal (VLoc l)
                                       ; store = Map.add_exn c.store ~key:l ~data:(Array.init n ~f:(fun _ -> vinit)) }
                 | _ -> failwith "Impossible by typing")
              | _ ->
                let open Syntax.Let_syntax in
                (match step { c with expr = spec.init } with
                 | None -> failwith "Impossible by typing"
                 | Some dc' ->
                   let%bind c' = dc' in
                   IDist.return { c' with expr = Mixed.EArrInit { spec with init = c'.expr } }))
           | _ ->
             let open Syntax.Let_syntax in
             (match step { c with expr = spec.size } with
              | None -> failwith "Impossible by typing"
              | Some dc' ->
                let%bind c' = dc' in
                IDist.return { c' with expr = Mixed.EArrInit { spec with size = c'.expr } }))
        | Mixed.EArrRead rd ->
          (match rd.addr with
           | Mixed.EVal vaddr ->
             (match rd.idx with
              | Mixed.EVal vidx ->
                (match vaddr with
                 | VLoc l ->
                   let storev = Map.find_exn c.store l in
                   (match vidx with
                    | VInt dn ->
                      let open Syntax.Let_syntax in
                      let%bind n = dn.value in
                      IDist.return { c with expr = Mixed.EVal storev.(n) }
                    | _ -> failwith "Impossible by typing")
                 | _ -> failwith "Impossible by typing")
              | _ ->
                let open Syntax.Let_syntax in
                (match step { c with expr = rd.idx } with
                 | None -> failwith "Impossible by typing"
                 | Some dc' ->
                   let%bind c' = dc' in
                   IDist.return { c' with expr = Mixed.EArrRead { rd with idx = c'.expr } }))
           | _ ->
             let open Syntax.Let_syntax in
             (match step { c with expr = rd.addr } with
              | None -> failwith "Impossible by typing"
              | Some dc' ->
                let%bind c' = dc' in
                IDist.return { c' with expr = Mixed.EArrRead { rd with addr = c'.expr } }))
        | _ -> failwith "TODO"
      end

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
