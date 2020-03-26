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
  | EAOp ao ->
    let (vs, cs) = List.split_while ~f:Runtime.is_value ao.args in
    let vs = Runtime.to_values vs in
    (match cs with
     | c' :: cs' ->
       (c', KAOp { op = ao.op ; evaluated = vs ; remaining = cs' })
     | _ -> failwith "Impossible")
  | EARel ar ->
    let (vs, cs) = List.split_while ~f:Runtime.is_value ar.args in
    let vs = Runtime.to_values vs in
    (match cs with
     | c' :: cs' ->
       (c', KARel { rel = ar.rel ; evaluated = vs ; remaining = cs' })
     | _ -> failwith "Impossible")
  | ETuple (e1, e2) ->
    if Runtime.is_value e1 then
      let v1 = Runtime.to_value e1 in
      (e2, KTupleR v1)
    else
      (e1, KTupleL e2)
  | ERecord fields ->
    let (vs, cs) = List.split_while ~f:(Fn.compose Runtime.is_value Tuple.T2.get2) fields in
    let vs = List.map ~f:(fun (x, c) -> (x, Runtime.to_value c)) vs in
    (match cs with
     | (x, c') :: cs' ->
       (c', KRecord { evaluated = vs ; x = x ; remaining = cs' })
     | __ -> failwith "Impossible")
  | EArrInit ai ->
    if Runtime.is_value ai.size then
      let vsize = Runtime.to_value ai.size in
      (ai.init, KArrInitInit vsize)
    else
      (ai.size, KArrInitSz ai.init)
  | EArrRead ar ->
    if Runtime.is_value ar.loc then
      let vloc = Runtime.to_value ar.loc in
      (ar.idx, KArrReadIdx vloc)
    else
      (ar.loc, KArrReadLoc ar.idx)
  | EArrWrite aw ->
    if Runtime.is_value aw.loc then
      let vloc = Runtime.to_value aw.loc in
      if Runtime.is_value aw.idx then
        let vidx = Runtime.to_value aw.idx in
        (aw.value, KArrWriteVal { loc = vloc ; idx = vidx })
      else
        (aw.idx, KArrWriteIdx { loc = vloc ; value = aw.value })
    else
      (aw.loc, KArrWriteLoc { idx = aw.idx ; value = aw.value })
  | EArrLen loc -> (loc, KArrLen)
  | EMux m ->
    if Runtime.is_value m.guard then
      let vguard = Runtime.to_value m.guard in
      if Runtime.is_value m.lhs then
        let vlhs = Runtime.to_value m.lhs in
        (m.rhs, KMuxR { guard = vguard ; lhs = vlhs })
      else
        (m.lhs, KMuxL { guard = vguard ; rhs = m.rhs })
    else
      (m.guard, KMuxGuard { lhs = m.lhs ; rhs = m.rhs })
  | EApp ap ->
    if Runtime.is_value ap.lam then
      let vlam = Runtime.to_value ap.lam in
      (ap.arg, KAppArg vlam)
    else
      (ap.lam, KAppLam ap.arg)
  | ELet l ->
    (l.value, KLet { pat = l.pat ; body = l.body })
  | EIf ite ->
    (ite.guard, KIf { thenb = ite.thenb ; elseb = ite.elseb })
  | EPrint p ->
    (p, KPrint)
  | _ -> failwith "Impossible"

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
