open Core

type 'v frame' =
  | KBOp         of { op : Boolean.Op.t ; evaluated : 'v Value.t List.t ; remaining : 'v Runtime.t List.t }
  | KAOp         of { op : Arith.Op.t ; evaluated : 'v Value.t List.t ; remaining : 'v Runtime.t List.t }
  | KARel        of { rel : Arith.Rel.t ; evaluated : 'v Value.t List.t ; remaining : 'v Runtime.t List.t }
  | KTupleL      of 'v Runtime.t
  | KTupleR      of 'v Value.t
  | KRecord      of { evaluated : (Var.t * 'v Value.t) List.t ; x : Var.t ; remaining : (Var.t * 'v Runtime.t) List.t }
  | KArrInitSz   of 'v Runtime.t
  | KArrInitInit of 'v Value.t
  | KArrReadLoc  of 'v Runtime.t
  | KArrReadIdx  of 'v Value.t
  | KArrWriteLoc of { idx : 'v Runtime.t ; value : 'v Runtime.t }
  | KArrWriteIdx of { loc : 'v Value.t ; value : 'v Runtime.t }
  | KArrWriteVal of { loc : 'v Value.t ; idx : 'v Value.t }
  | KArrLen
  | KMuxGuard    of { lhs : 'v Runtime.t ; rhs : 'v Runtime.t }
  | KMuxL        of { guard : 'v Value.t ; rhs : 'v Runtime.t }
  | KMuxR        of { guard : 'v Value.t ; lhs : 'v Value.t }
  | KAppLam      of 'v Runtime.t
  | KAppArg      of 'v Value.t
  | KLet         of { pat : Pattern.t ; body : 'v Runtime.t }
  | KIf          of { thenb : 'v Runtime.t ; elseb : 'v Runtime.t }
  | KPrint

type 'v frame = { source_location : Section.t
                ; datum : 'v frame' }

type 'v t = 'v frame List.t

let decompose (c : 'v Runtime.t') : ('v Runtime.t * 'v frame') =
  match c with
  | EBOp bo ->
    let (vs, cs) = List.split_while ~f:Runtime.is_value bo.args in
    let vs = Runtime.to_values vs in
    (match cs with
     | c' :: cs' ->
       (c', KBOp { op = bo.op ; evaluated = vs ; remaining = cs' })
     | _ -> failwith "Impossible")
  | _ -> failwith "TODO"

let push (c : 'v Runtime.t) (k : 'v t) : ('v Runtime.t * 'v t) =
  assert (not (Runtime.is_redex c));
  assert (not (Runtime.is_value c));
  let (c', kf') = decompose c.datum in
  (c', { source_location = c.source_location ; datum = kf' } :: k)

let compose (c : 'v Runtime.t) (kf : 'v frame') : 'v Runtime.t' =
  match kf with
  | KBOp bo ->
    let vs = Runtime.of_values bo.evaluated in
    let cs = vs @ [ c ] @ bo.remaining in
    EBOp { op = bo.op ; args = cs }
  | _ -> failwith "TODO"

let pop (c : 'v Runtime.t) (k : 'v t) : ('v Runtime.t * 'v t) =
  assert (Runtime.is_value c);
  assert (not (List.is_empty k));
  match k with
  | kf :: k' -> ({ source_location = kf.source_location ; datum = compose c kf.datum }, k')
  | _ -> failwith "Impossible"
