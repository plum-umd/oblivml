open Core

type env = (Var.t, Mixed.value, Var.comparator_witness) Map.t
type store = (Loc.t, Mixed.value Array.t, Loc.comparator_witness) Map.t

type config = { n     : Int.t
              ; env   : env
              ; store : store
              ; expr  : Mixed.t
              ; cont  : Mixed.ectx
              }

type trace = config List.t

module Syntax = Monad.Make(IDist)

(** Erase a source term into a runtime term *)
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
  | ETrust x -> EVar { path = [x] }
  | EProve x -> EVar { path = [x] }
  | EMux m -> EMux { guard = of_source m.guard ; lhs = of_source m.lhs ; rhs = of_source m.rhs }
  | EAbs f -> EAbs { param = f.param ; body = of_source f.body }
  | ERec f -> ERec { name = f.name ; param = f.param ; body = of_source f.body }
  | EApp ap -> EApp { lam = of_source ap.lam ; arg = of_source ap.arg }
  | ELet l -> ELet { pat = l.pat ; value = of_source l.value ; body = of_source l.body }
  | EType t -> of_source t.body
  | EIf ite -> EIf { guard = of_source ite.guard ; thenb = of_source ite.thenb ; elseb = of_source ite.elseb }

(** Pop an evaluation context from the stack, and plug `e` into it *)
let compose1 (e : Mixed.t) (k : Mixed.ectx) : (Mixed.t * Mixed.ectx) Option.t =
  match k with
  | KHole -> None
  | KBUnOp buo -> Some (EBUnOp { op = buo.op ; arg = e }, buo.cont)
  | KBBinOpL bbo -> Some (EBBinOp { op = bbo.op ; lhs = e ; rhs = bbo.rhs }, bbo.cont)
  | KBBinOpR bbo -> Some (EBBinOp { op = bbo.op ; lhs = EVal bbo.lhs ; rhs = e }, bbo.cont)
  | KAUnOp auo -> Some (EAUnOp { op = auo.op ; arg = e }, auo.cont)
  | KABinOpL abo -> Some (EABinOp { op = abo.op ; lhs = e ; rhs = abo.rhs }, abo.cont)
  | KABinOpR abo -> Some (EABinOp { op = abo.op ; lhs = EVal abo.lhs ; rhs = e }, abo.cont)
  | KAUnRel aur -> Some (EAUnRel { rel = aur.rel ; arg = e }, aur.cont)
  | KABinRelL abr -> Some (EABinRel { rel = abr.rel ; lhs = e ; rhs = abr.rhs }, abr.cont)
  | KABinRelR abr -> Some (EABinRel { rel = abr.rel ; lhs = EVal abr.lhs ; rhs = e }, abr.cont)
  | KTupleL (cont, r) -> Some (ETuple (e, r), cont)
  | KTupleR (l, cont) -> Some (ETuple (EVal l, e), cont)
  | KRecord (vs, (x, cont), es) -> Some (ERecord ((List.map ~f:(fun (x, v) -> (x, Mixed.EVal v)) vs) @ [(x, e)] @ es), cont)
  | KArrInitSz sz -> Some (EArrInit { size = e ; init = sz.init }, sz.cont)
  | KArrInitV v -> Some (EArrInit { size = EVal v.size ; init = e }, v.cont)
  | KArrReadAddr rd -> Some (EArrRead { addr = e ; idx = rd.idx }, rd.cont)
  | KArrReadIdx rd -> Some (EArrRead { addr = EVal rd.addr ; idx = e }, rd.cont)
  | KArrWriteAddr wr -> Some (EArrWrite { addr = e ; idx = wr.idx ; value = wr.value }, wr.cont)
  | KArrWriteIdx wr -> Some (EArrWrite { addr = EVal wr.addr ; idx = e ; value = wr.value }, wr.cont)
  | KArrWriteVal wr -> Some (EArrWrite { addr = EVal wr.addr ; idx = EVal wr.idx ; value = e }, wr.cont)
  | KArrLen l -> Some (EArrLen e, l.cont)
  | KMuxGuard m -> Some (EMux { guard = e ; lhs = m.lhs ; rhs = m.rhs }, m.cont)
  | KMuxL m -> Some (EMux { guard = EVal m.guard ; lhs = e ; rhs = m.rhs }, m.cont)
  | KMuxR m -> Some (EMux { guard = EVal m.guard ; lhs = EVal m.lhs ; rhs = e }, m.cont)
  | KAppF ap -> Some (EApp { lam = e ; arg = ap.arg }, ap.cont)
  | KAppA ap -> Some (EApp { lam = EVal ap.lam ; arg = e }, ap.cont)
  | KLet l -> Some (ELet { pat = l.pat ; value = e ; body = l.body }, l.cont)
  | KIf ite -> Some (EIf { guard = e ; thenb = ite.thenb ; elseb = ite.elseb }, ite.cont)

(** Decompose an `e` into a redex and an evaluation stack *)
let rec decompose (e : Mixed.t) (k : Mixed.ectx) : (Mixed.redex * Mixed.ectx) Option.t =
  match e with
  | ELit l -> Some (RLit { value = l.value ; label = l.label }, k)
  | EVal v -> (* I don't really want this here, but it is fine for now *)
    let open Option.Let_syntax in
    let%bind (e', k') = compose1 e k in
    decompose e' k'
  | EFlip -> Some (RFlip, k)
  | ERnd -> Some (RRnd, k)
  | EVar p -> Some (RVar { path = p.path }, k)
  | EBUnOp buo ->
    (match buo.arg with
     | EVal v -> Some (RBUnOp { op = buo.op ; arg = v }, k)
     | _ -> decompose buo.arg (KBUnOp { op = buo.op ; cont = k }))
  | EBBinOp bbo ->
    (match bbo.lhs with
     | EVal v1 ->
       (match bbo.rhs with
        | EVal v2 -> Some (RBBinOp { op = bbo.op ; lhs = v1 ; rhs = v2 }, k)
        | _ -> decompose bbo.rhs (KBBinOpR { op = bbo.op ; lhs = v1 ; cont = k }))
     | _ -> decompose bbo.lhs (KBBinOpL { op = bbo.op ; cont = k ; rhs = bbo.rhs }))
  | EAUnOp auo ->
    (match auo.arg with
     | EVal v -> Some (RAUnOp { op = auo.op ; arg = v }, k)
     | _ -> decompose auo.arg (KAUnOp { op = auo.op ; cont = k}))
  | EABinOp abo ->
    (match abo.lhs with
     | EVal v1 ->
       (match abo.rhs with
        | EVal v2 -> Some (RABinOp { op = abo.op ; lhs = v1 ; rhs = v2 }, k)
        | _ -> decompose abo.rhs (KABinOpR { op = abo.op ; lhs = v1 ; cont = k }))
     | _ -> decompose abo.lhs (KABinOpL { op = abo.op ; cont = k ; rhs = abo.rhs }))
  | EAUnRel aur ->
    (match aur.arg with
     | EVal v -> Some (RAUnRel { rel = aur.rel ; arg = v }, k)
     | _ -> decompose aur.arg (KAUnRel { rel = aur.rel ; cont = k }))
  | EABinRel abr ->
    (match abr.lhs with
     | EVal v1 ->
       (match abr.rhs with
        | EVal v2 -> Some (RABinRel { rel = abr.rel ; lhs = v1 ; rhs = v2 }, k)
        | _ -> decompose abr.rhs (KABinRelR { rel = abr.rel ; lhs = v1 ; cont = k }))
     | _ -> decompose abr.lhs (KABinRelL { rel = abr.rel ; cont = k ; rhs = abr.rhs }))
  | ETuple (l, r) ->
    (match l with
     | EVal v1 ->
       (match r with
        | EVal v2 -> Some (RTuple (v1, v2), k)
        | _ -> decompose r (KTupleR (v1, k)))
     | _ -> decompose l (KTupleL (k, r)))
  | ERecord fields ->
    let (vals, exprs) = List.split_while ~f:(fun (_, e) -> match e with | EVal _ -> true | _ -> false) fields in
    let vals =
      List.map
        ~f:(fun (x, e) ->
            match e with
            | EVal v -> (x, v)
            | _ -> failwith "Impossible by `split_while` above")
        vals
    in
    (match exprs with
     | [] -> Some (RRecord vals, k)
     | (x, e) :: exprs -> decompose e (KRecord (vals, (x, k), exprs)))
  | EArrInit sp ->
    (match sp.size with
     | EVal vsize ->
       (match sp.init with
        | EVal vinit -> Some (RArrInit { size = vsize ; init = vinit }, k)
        | _ -> decompose sp.init (KArrInitV { size = vsize ; cont = k }))
     | _ -> decompose sp.size (KArrInitSz { cont = k ; init = sp.init }))
  | EArrRead sp ->
    (match sp.addr with
     | EVal vaddr ->
       (match sp.idx with
        | EVal vidx -> Some (RArrRead { addr = vaddr ; idx = vidx }, k)
        | _ -> decompose sp.idx (KArrReadIdx { addr = vaddr ; cont = k }))
     | _ -> decompose sp.addr (KArrReadAddr { cont = k ; idx = sp.idx }))
  | EArrWrite sp ->
    (match sp.addr with
     | EVal vaddr ->
       (match sp.idx with
        | EVal vidx ->
          (match sp.value with
           | EVal v -> Some (RArrWrite { addr = vaddr ; idx = vidx ; value = v }, k)
           | _ -> decompose sp.value (KArrWriteVal { addr = vaddr ; idx = vidx ; cont = k }))
        | _ -> decompose sp.idx (KArrWriteIdx { addr = vaddr ; cont = k ; value = sp.value }))
     | _ -> decompose sp.addr (KArrWriteAddr { cont = k ; idx = sp.idx ; value = sp.value }))
  | EArrLen l ->
    (match l with
     | EVal v -> Some (RArrLen v, k)
     | _ -> decompose l (KArrLen { cont = k }))
  | EUse x -> Some (RUse x, k)
  | EReveal x -> Some (RReveal x, k)
  | EMux m ->
    (match m.guard with
     | EVal vguard ->
       (match m.lhs with
        | EVal vlhs ->
          (match m.rhs with
           | EVal vrhs -> Some (RMux { guard = vguard ; lhs = vlhs ; rhs = vrhs }, k)
           | _ -> decompose m.rhs (KMuxR { guard = vguard ; lhs = vlhs ; cont = k }))
        | _ -> decompose m.lhs (KMuxL { guard = vguard ; cont = k ; rhs = m.rhs }))
     | _ -> decompose m.guard (KMuxGuard { cont = k ; lhs = m.lhs ; rhs = m.rhs }))
  | EAbs a -> Some (RAbs { param = a.param ; body = a.body }, k)
  | ERec r -> Some (RRec { name = r.name ; param = r.param ; body = r.body }, k)
  | EApp ap ->
    (match ap.lam with
     | EVal vf ->
       (match ap.arg with
        | EVal varg -> Some (RApp { lam = vf ; arg = varg }, k)
        | _ -> decompose ap.arg (KAppA { lam = vf ; cont = k }))
     | _ -> decompose ap.lam (KAppF { cont = k ; arg = ap.arg }))
  | ELet l ->
    (match l.value with
     | EVal v -> Some (RLet { pat = l.pat ; value = v ; body = l.body }, k)
     | _ -> decompose l.value (KLet { pat = l.pat ; cont = k ; body = l.body }))
  | EIf ite ->
    (match ite.guard with
     | EVal v -> Some (RIf { guard = v ; thenb = ite.thenb ; elseb = ite.elseb }, k)
     | _ -> decompose ite.guard (KIf { cont = k ; thenb = ite.thenb ; elseb = ite.elseb }))

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


let step_redex n env store (r : Mixed.redex) =
  match r with
  | RLit l ->
    let (v : Mixed.value) =
      match l.value with
      | LitUnit () -> VUnit (IDist.return ())
      | LitBool b  -> VBool { value = (IDist.return b) ; label = l.label }
      | LitInt n -> VInt { value = (IDist.return n) ; label = l.label }
    in
    IDist.return (n, env, store, Mixed.EVal v)
  | RFlip -> IDist.return (n + 1, env, store, Mixed.EVal (Mixed.VFlip (IDist.bit n)))
  | RRnd ->
    (* TODO(ins): Should parameterize this better *)
    let bitwidth = 4 in
    let open Syntax.Let_syntax in
    let%bind word = Syntax.all (List.init bitwidth ~f:(fun idx -> IDist.bit (n + idx))) in
    let n =
      List.fold_right
        ~init:Int.zero
        ~f:(fun b acc -> Int.bit_or (if b then Int.one else Int.zero) (Int.shift_left acc 1))
        word
    in
    IDist.return (n + bitwidth, env, store, Mixed.EVal (Mixed.VRnd (IDist.return n)))
  | RVar p -> IDist.return (n, env, store, Mixed.EVal (lookup env p.path))
  | RBUnOp buo ->
    (match buo.arg with
     | VBool b ->
       (match buo.op with
        | Boolean.Un.Op.Not -> IDist.return (n, env, store, Mixed.EVal (Mixed.VBool { b with value = (IDist.negate b.value) })))
     | _ -> failwith "Impossible by typing")
  | RBBinOp bbo ->
    (match bbo.lhs, bbo.rhs with
        | VBool b1, VBool b2 ->
          (match bbo.op with
           | Boolean.Bin.Op.And -> IDist.return (n, env, store, Mixed.EVal (Mixed.VBool { value = (IDist.bitand b1.value b2.value) ; label = Label.join b1.label b2.label })))
        | _ -> failwith "Impossible by typing")
  | RAUnOp _ -> failwith "TODO"
  | RABinOp _ -> failwith "TODO"
  | RAUnRel _ -> failwith "TODO"
  | RABinRel _ -> failwith "TODO"
  | RTuple (lhs, rhs) -> IDist.return (n, env, store, Mixed.EVal (Mixed.VTuple (lhs, rhs)))
  | RRecord fields -> IDist.return (n, env, store, Mixed.EVal (Mixed.VRecord fields))
  | RArrInit sp ->
    (match sp.size with
     | VInt dn ->
       let open Syntax.Let_syntax in
       let%bind n = dn.value in
       let l = Loc.fresh () in
       let store' = Map.add_exn store ~key:l ~data:(Array.init n ~f:(fun _ -> sp.init)) in
       IDist.return (n, env, store', Mixed.EVal (Mixed.VLoc l))
     | _ -> failwith "Impossible by typing")
  | RArrRead sp ->
    (match sp.addr, sp.idx with
     | VLoc l, VInt dn ->
       let storev = Map.find_exn store l in
       let open Syntax.Let_syntax in
       let%bind n = dn.value in
       IDist.return (n, env, store, Mixed.EVal (storev.(n)))
     | _ -> failwith "Impossible by typing")
  | RArrWrite sp ->
    (match sp.addr, sp.idx with
     | VLoc l, VInt dn ->
       let storev = Map.find_exn store l in
       let ret = storev.(n) in
       storev.(n) <- sp.value;
       let open Syntax.Let_syntax in
       let%bind n = dn.value in
       IDist.return (n, env, store, Mixed.EVal (ret))
     | _ -> failwith "Impossible by typing")
  | RArrLen l ->
    (match l with
     | VLoc l ->
       let storev = Map.find_exn store l in
       IDist.return (n, env, store, Mixed.EVal (Mixed.VInt { value = IDist.return (Array.length storev) ; label = Label.public }))
     | _ -> failwith "Impossible by typing")
  | RUse x ->
    let v = Map.find_exn env x in
    (match v with
     | VFlip b -> IDist.return (n, env, store, Mixed.EVal (Mixed.VBool { value = b ; label = Label.secret }))
     | VRnd r -> IDist.return (n, env, store, Mixed.EVal (Mixed.VInt { value = r ; label = Label.secret }))
     | _ -> failwith "Impossible by typing")
  | RReveal x ->
    let v = Map.find_exn env x in
    (match v with
     | VFlip b ->
       let open Syntax.Let_syntax in
       let%bind b = b in
       IDist.return (n, env, store, Mixed.EVal (Mixed.VBool { value = IDist.return b ; label = Label.public }))
     | VRnd r ->
       let open Syntax.Let_syntax in
       let%bind n = r in
       IDist.return (n, env, store, Mixed.EVal (Mixed.VInt { value = IDist.return n ; label = Label.public }))
     | _ -> failwith "Impossible by typing")
  | RMux mux ->
    (match mux.guard with
     | VBool bguard -> failwith "TODO"
     | _ -> failwith "Impossible by typing")
  | RAbs lam -> IDist.return (n, env, store, Mixed.EVal (Mixed.VAbs { env = env ; param = lam.param ; body = lam.body }))
  | RRec lam -> IDist.return (n, env, store, Mixed.EVal (Mixed.VRec { env = env ; name = lam.name ; param = lam.param ; body = lam.body }))
(*  | RApp ap ->
    (match ap.lam with
     | VAbs abs ->
       let env' = failwith "TODO" in
       IDist.return (n, env', store, abs.body))
     let env' = failwith "TODO" in *)
  | _ -> failwith "TODO"

let step (c : config) : (config IDist.t) Option.t =
  Option.bind
    (decompose c.expr c.cont)
    ~f:(fun (r, k) ->
        Option.return
          (IDist.bind
             (step_redex c.n c.env c.store r)
             ~f:(fun (n, env, store, e) ->
                 IDist.return { n = n ; env = env ; store = store ; expr = e ; cont = k })))

let rec eval' (dt : trace IDist.t) (c : config) : trace IDist.t =
  let open Syntax.Let_syntax in
  let%bind t = dt in
  let dt' = IDist.return (c :: t) in
  match step c with
  | None     -> dt'
  | Some d ->
    let%bind c' = d in
    eval' dt' c'

let eval e = eval' (IDist.return []) { n = 0 ; env = Map.empty (module Var) ; store = Map.empty (module Loc) ; cont = KHole ; expr = e }
