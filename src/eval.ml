open Core

type env = (Var.t, Sampling.value Ref.t, Var.comparator_witness) Map.t
type store = (Loc.t, Sampling.value Array.t, Loc.comparator_witness) Map.t

type region = (Int.t, Int.comparator_witness) Set.t
let empty_region = Set.empty (module Int)

let region_tracker = Hashtbl.create (module Int)
let dyn_region_gen = ref 0
let next_region (Some loc) =
  let ret = !dyn_region_gen in
  dyn_region_gen := ret + 1;
  Hashtbl.add_exn region_tracker ~key:ret ~data:loc;
  Set.singleton (module Int) ret

let report () =
  Hashtbl.iteri
    region_tracker
    ~f:(fun ~key ~data ->
        print_endline (Printf.sprintf "%d-%s" key (Section.to_string data)))

type config = { env   : env
              ; store : store
              ; expr  : Sampling.t
              }

let rec lookup (v : Sampling.value) p : Sampling.value =
  List.fold_left
    ~init:v
    ~f:(fun acc field ->
        match acc with
        | Sampling.VRecord fields ->
          let m = Map.of_alist_exn (module Var) fields in Map.find_exn m field
        | _ -> failwith "Attempt to lookup field in non-record")
    p

let disjoint_union m1 m2 =
  Map.merge m1 m2
    ~f:(fun ~key:x overlap ->
        match overlap with
        | `Left v1       -> (Some v1)
        | `Right v2      -> (Some v2)
        | `Both (v1, v2) -> failwith "Disjoint Union of overlapping maps")

let rec pattern_match (p : Pattern.t) v =
  match p, v with
  | (XWild, _)                               -> Map.empty (module Var)
  | (XVar x, _)                              -> Map.singleton (module Var) x (ref v)
  | (XTuple (p1, p2), Sampling.VTuple (v1, v2)) ->
    let bs1 = pattern_match p1 v1 in
    let bs2 = pattern_match p2 v2 in
    disjoint_union bs1 bs2
  | (XRecord ps, Sampling.VRecord bindings)     ->
    List.fold2_exn
      ~init:(Map.empty (module Var))
      ~f:(fun acc (x, p) (y, v) ->
          assert(Var.equal x y); (* Sanity check *)
          let bs = pattern_match p v in
          disjoint_union acc bs)
      ps
      bindings
  | (XAscr (p', _), v) -> pattern_match p' v
  | _ -> failwith "Pattern did not match value"

let apply env param v =
  let binds = pattern_match param v in
  Map.merge env binds
    ~f:(fun ~key:x overlap ->
        match overlap with
        | `Left v1       -> (Some v1)
        | `Right v2      -> (Some v2)
        | `Both (v1, v2) -> (Some v2))

let region_to_string r =
  Printf.sprintf "{ %s }" (String.concat ~sep:", " (List.map ~f:Int.to_string (Set.to_list r)))

let rec check_regions r l (v1 : Sampling.value) (v2 : Sampling.value) : (Sampling.value * Sampling.value) Option.t =
  let open Option.Let_syntax in
  match v1, v2 with
  | VUnit, VUnit       -> Some (VUnit, VUnit)
  | VBool b1, VBool b2 ->
    let l' = Label.join l (Label.join b1.label b2.label) in
    let r' = Set.union_list (module Int) [ r ; b1.region ; b2.region ] in
    let v1' = Sampling.VBool { value = b1.value ; label = l' ; region = r' } in
    let v2' = Sampling.VBool { value = b2.value ; label = l' ; region = r' } in
    Some (v1', v2')
  | VInt n1, VInt n2 ->
    let l' = Label.join l (Label.join n1.label n2.label) in
    let r' = Set.union_list (module Int) [ r ; n1.region ; n2.region ] in
    let v1' = Sampling.VInt { value = n1.value ; label = l' ; region = r' } in
    let v2' = Sampling.VInt { value = n2.value ; label = l' ; region = r' } in
    Some (v1', v2')
  | VFlip f1, VFlip f2 ->
    let check_lhs = Set.is_subset r f1.region && not (Set.equal r f1.region) in
    let check_rhs = Set.is_subset r f2.region && not (Set.equal r f2.region) in
    if check_lhs && check_rhs then
        let r' = Set.union_list (module Int) [ r ; f1.region ; f2.region ] in
        let v1' = Sampling.VFlip { value = f1.value ; region = r' } in
        let v2' = Sampling.VFlip { value = f2.value ; region = r' } in
        Some (v1', v2')
    else
      begin
        Printf.printf "%s\n" (region_to_string r);
        Printf.printf "%s\n" (region_to_string f1.region);
        Printf.printf "%s\n" (region_to_string f2.region);
        None
      end
  | VRnd r1, VRnd r2 ->
    let check_lhs = Set.is_subset r r1.region && not (Set.equal r r1.region) in
    let check_rhs = Set.is_subset r r2.region && not (Set.equal r r2.region) in
    if check_lhs && check_rhs then
      let r' = Set.union_list (module Int) [ r ; r1.region ; r2.region ] in
      let v1' = Sampling.VRnd { value = r1.value ; region = r' } in
      let v2' = Sampling.VRnd { value = r2.value ; region = r' } in
      Some (v1', v2')
    else
      begin
        Printf.printf "%s\n" (region_to_string r);
        Printf.printf "%s\n" (region_to_string r1.region);
        Printf.printf "%s\n" (region_to_string r2.region);
        None
      end
  | VTuple (v11, v12), VTuple (v21, v22) ->
    let%bind (v11', v21') = check_regions r l v11 v21 in
    let%bind (v12', v22') = check_regions r l v12 v22 in
    let v1' = Sampling.VTuple (v11', v12') in
    let v2' = Sampling.VTuple (v21', v22') in
    Some (v1', v2')
  | VRecord fields1, VRecord fields2 ->
    let%bind (fields1', fields2') =
      List.fold2_exn
        ~init:(Some ([], []))
        ~f:(fun acc (x, v1) (y, v2) ->
            assert(Var.equal x y); (* Sanity check *)
            let%bind (left, right) = acc in
            let%bind (v1', v2') = check_regions r l v1 v2 in
            Some ((x, v1') :: left, (x, v2') :: right))
        fields1
        fields2
    in
    Some (Sampling.VRecord (List.rev fields1'), Sampling.VRecord (List.rev fields2'))
  | _ -> failwith "Different kinds of values in mux, or unsupported data type"

let rec eval' (c : config) : Sampling.value * store =
  match c.expr.node with
  | ELit l ->
    (match l.value with
     | Literal.LitUnit () -> (Sampling.VUnit, c.store)
     | Literal.LitBool b -> (Sampling.VBool { value = b ; label = l.label ; region = empty_region }, c.store)
     | Literal.LitInt n -> (Sampling.VInt { value = n ; label = l.label ; region = empty_region }, c.store))
  | EFlip -> (Sampling.VFlip { value = (Random.bool ()) ; region = next_region c.expr.loc }, c.store)
  | ERnd -> (Sampling.VRnd { value = (Random.bits ()) ; region = next_region c.expr.loc }, c.store)
  | EVar x ->
    (match x.path with
     | [] -> failwith "Impossible"
     | x :: fields ->
       let v = !(Map.find_exn c.env x) in
       (lookup v fields, c.store))
  | EBUnOp buo ->
    let (v, s) = eval' { c with expr = buo.arg } in
    (Sampling.denote_buo buo.op v, s)
  | EBBinOp bbo ->
    let (v1, s1) = eval' { c with expr = bbo.lhs } in
    let (v2, s2) = eval' { c with store = s1 ; expr = bbo.rhs } in
    (Sampling.denote_bbo bbo.op v1 v2, s2)
  | EAUnOp auo ->
    let (v, s) = eval' { c with expr = auo.arg } in
    (Sampling.denote_auo auo.op v, s)
  | EABinOp abo ->
    let (v1, s1) = eval' { c with expr = abo.lhs } in
    let (v2, s2) = eval' { c with store = s1 ; expr = abo.rhs } in
    (Sampling.denote_abo abo.op v1 v2, s2)
  | EAUnRel aur ->
    let (v, s) = eval' { c with expr = aur.arg } in
    (Sampling.denote_aur aur.rel v, s)
  | EABinRel abr ->
    let (v1, s1) = eval' { c with expr = abr.lhs } in
    let (v2, s2) = eval' { c with store = s1 ; expr = abr.rhs } in
    (Sampling.denote_abr abr.rel v1 v2, s2)
  | ETuple (l, r) ->
    let (v1, s1) = eval' { c with expr = l } in
    let (v2, s2) = eval' { c with store = s1 ; expr = r } in
    (Sampling.VTuple (v1, v2), s2)
  | ERecord fields ->
    let (s', vs) =
      List.fold_map
        ~init:c.store
        ~f:(fun s (x, curr) ->
            let (v1, s1) = eval' { c with store = s ; expr = curr } in
            (s1, (x, v1)))
        fields
    in
    (Sampling.VRecord vs, s')
  | EArrInit spec ->
    let (v1, s1) = eval' { c with expr = spec.size } in
    let (v2, s2) = eval' { c with store = s1 ; expr = spec.init } in
    (match v1, v2 with
     | (VInt n, VAbs f) ->
       let (s', contents) =
         Array.fold_map
           ~init:c.store
           ~f:(fun s n ->
               let env' = apply f.env f.param (Sampling.VInt { value = n ; label = Label.public ; region = empty_region }) in
               let (v, s1) = eval' { env = env' ; store = s ; expr = f.body } in
               (s1, v))
           (Array.init n.value ~f:Fn.id)
       in
       let l = Loc.fresh () in
       let s'' = Map.add_exn s' ~key:l ~data:contents in
       (Sampling.VLoc l, s'')
     | _ -> failwith "ArrInit type error")
  | EArrRead spec ->
    let (v1, s1) = eval' { c with expr = spec.addr } in
    let (v2, s2) = eval' { c with store = s1 ; expr = spec.idx } in
    (match v1, v2 with
     | VLoc l, VInt n ->
       let storev = Map.find_exn s2 l in
       (storev.(n.value), s2)
     | _ -> failwith "ArrRead type error")
  | EArrWrite spec ->
    let (v1, s1) = eval' { c with expr = spec.addr } in
    let (v2, s2) = eval' { c with store = s1 ; expr = spec.idx } in
    let (v3, s3) = eval' { c with store = s2 ; expr = spec.value } in
    (match v1, v2 with
     | VLoc l, VInt n ->
       let storev = Map.find_exn s3 l in
       let result = storev.(n.value) in
       storev.(n.value) <- v3;
       (result, s3)
     | _ -> failwith "ArrWrite type error")
  | EArrLen addr ->
    let (v, s) = eval' { c with expr = addr } in
    (match v with
     | VLoc l ->
       let storev = Map.find_exn s l in
       (VInt { value = Array.length storev ; label = Label.public ; region = empty_region }, s)
     | _ -> failwith "ArrLen type error")
  | EUse x ->
    let v = !(Map.find_exn c.env x) in
    (match v with
     | VFlip flip -> (VBool { value = flip.value ; label = Label.secret ; region = flip.region }, c.store)
     | VRnd rnd -> (VInt { value = rnd.value ; label = Label.secret ; region = rnd.region }, c.store)
     | _ -> failwith "Use type error")
  | EReveal x ->
    let v = !(Map.find_exn c.env x) in
    (match v with
     | VFlip flip -> (VBool { value = flip.value ; label = Label.public ; region = empty_region }, c.store)
     | VRnd rnd -> (VInt { value = rnd.value ; label = Label.public ; region = rnd.region }, c.store)
     | _ -> failwith "Reveal type error")
  | EMux mux ->
    let (v1, s1) = eval' { c with expr = mux.guard } in
    let (v2, s2) = eval' { c with store = s1 ; expr = mux.lhs } in
    let (v3, s3) = eval' { c with store = s2 ; expr = mux.rhs } in
    (match v1 with
     | VBool b ->
       (match check_regions b.region b.label v2 v3 with
        | None ->
          let Some loc = c.expr.loc in
          report ();
          let msg = Printf.sprintf "Region check failed at %s\n" (Section.to_string loc) in failwith msg
        | Some (v2', v3') ->
          if b.value then (VTuple (v2', v3'), s3) else (VTuple (v3', v2'), s3))
     | _ -> failwith "Mux type error")
  | EAbs f ->
    (VAbs { env = c.env ; param = f.param ; body = f.body }, c.store)
  | ERec f ->
    let bogus : Sampling.value Ref.t = ref (Sampling.VUnit) in
    let lam = Sampling.VAbs { env = Map.set c.env ~key:f.name ~data:bogus ; param = f.param ; body = f.body } in
    bogus := lam;
    (lam, c.store)
  | EApp ap ->
    let (v1, s1) = eval' { c with expr = ap.lam } in
    let (v2, s2) = eval' { c with store = s1 ; expr = ap.arg } in
    (match v1 with
     | VAbs f ->
       eval' { env = apply f.env f.param v2 ; store = s2 ; expr = f.body }
     | _ -> failwith "App type error")
  | ELet b ->
    let (v1, s1) = eval' { c with expr = b.value } in
    eval' { env = apply c.env b.pat v1 ; store = s1 ; expr = b.body }
  | EIf ite ->
    let (v1, s1) = eval' { c with expr = ite.guard } in
    (match v1 with
     | VBool b ->
       if b.value then
         eval' { c with store = s1 ; expr = ite.thenb }
       else
         eval' { c with store = s1 ; expr = ite.elseb }
     | _ -> failwith "If type error")
  | EPrint e ->
    let (v1, s1) = eval' { c with expr = e } in
    Printf.printf "%s\n" (Sampling.value_to_string v1);
    (v1, s1)


let eval e = eval' { env = Map.empty (module Var) ; store = Map.empty (module Loc) ; expr = Sampling.of_source e }
