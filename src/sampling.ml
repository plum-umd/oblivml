open Core

module Reduction : Eval.Reduction =
struct
  open Runtime

  type value =
    | VUnit
    | VBool   of { value : Bool.t
                 ; label : Label.t
                 ; region : Region.t }
    | VInt    of { value : Int.t
                 ; label : Label.t
                 ; region : Region.t }
    | VFlip   of { value : Bool.t
                 ; region : Region.t }
    | VRnd    of { value : Int.t
                 ; region : Region.t }
    | VLoc    of Loc.t
    | VAbs    of { env : (Var.t, value Ref.t, Var.comparator_witness) Map.t
                 ; pat : Pattern.t
                 ; body : value Runtime.t }
    | VTuple  of (value, value) Tuple.T2.t
    | VRecord of (Var.t * value) list

  let value_to_string' = function
    | VUnit -> "()"
    | VBool b ->
      Printf.sprintf "Bool(%s<%s,%s>)"
        (Bool.to_string b.value)
        (Label.to_string b.label)
        (Region.to_string b.region)
    | VInt n ->
      Printf.sprintf "Int(%s<%s,%s>)"
        (Int.to_string n.value)
        (Label.to_string n.label)
        (Region.to_string n.region)
    | VFlip f ->
      Printf.sprintf "Flip(%s<%s>)"
        (Bool.to_string f.value)
        (Region.to_string f.region)
    | VRnd r ->
      Printf.sprintf "Rnd(%s<%s>)"
        (Int.to_string r.value)
        (Region.to_string r.region)
    | _ -> failwith "Unimplemented"

  let value_to_string v = Value.to_string v value_to_string'

  type env = (Var.t, value Ref.t, Var.comparator_witness) Map.t

  let env_to_string e = "{ TODO }"

  type store = (Loc.t, value Array.t, Loc.comparator_witness) Map.t

  let store_to_string s = "{ TODO }"

  type state = { e : env ; s : store }

  let empty = { e = Map.empty (module Var) ; s = Map.empty (module Loc) }

  let denote_lit d l =
    match d with
    | Literal.LitUnit () -> VUnit
    | Literal.LitBool b -> VBool { value = b ; label = l ; region = Region.bottom }
    | Literal.LitInt n -> VInt { value = n ; label = l ; region = Region.bottom }

  let denote_flip r = VFlip { value = (Random.bool ()) ; region = r }

  let denote_rnd r = VRnd { value = (Random.bits ()) ; region = r }

  let rec denote_access v accessors =
    match accessors with
    | [] -> Or_error.return v
    | f :: accessors' ->
      (match v with
       | VRecord fields ->
         (match List.Assoc.find ~equal:Var.equal fields f with
          | Some v' -> denote_access v' accessors'
          | None    ->
            let msg =
              Printf.sprintf "The record %s does not have a field %s."
                (value_to_string' v)
                (Var.to_string f)
            in
            Or_error.error_string msg)
       | _ ->
         let msg =
           Printf.sprintf "Attempted to access field %s of non-record value %s."
             (Var.to_string f)
             (value_to_string' v)
         in
         Or_error.error_string msg)

  let denote_var x acs e =
    let mr = Map.find e x in
    match mr with
    | Some r -> let r = !r in denote_access r acs
    | None   ->
      let msg =
        Printf.sprintf "Variable %s not found in environment %s."
          (Var.to_string x)
          (env_to_string e)
      in
      Or_error.error_string msg

  let denote_not v =
    match v.Value.datum with
    | VBool b -> Or_error.return (VBool { b with value = not b.value })
    | _ ->
      let msg =
        Printf.sprintf "Attempted to negate non-boolean value, %s."
          (value_to_string v)
      in
      Or_error.error_string msg

  let denote_and l r =
    match l.Value.datum with
    | VBool bl ->
      (match r.Value.datum with
       | VBool br -> Or_error.return (VBool { value = bl.value && br.value ; label = Label.join bl.label br.label ; region = Region.join bl.region br.region })
       | _ ->
         let msg =
           Printf.sprintf "Attempted to logical-and (&&) non-boolean value, %s [RHS]."
             (value_to_string r)
         in
         Or_error.error_string msg)
    | _ ->
      let msg =
        Printf.sprintf "Attempted to logical-and (&&) non-boolean value, %s [LHS]."
          (value_to_string l)
      in
      Or_error.error_string msg

  let denote_bop op args =
    match op with
    | Boolean.Op.Not ->
      (match args with
       | [ b ] -> denote_not b
       | _     -> failwith "Impossible, forbidden by lexer / parser.")
    | Boolean.Op.And ->
      (match args with
       | [ l ; r ] -> denote_and l r
       | _         -> failwith "Impossible, forbidden by lexer / parser.")

  let denote_aop op args =
    failwith "TODO -- there are many ops and they are so so boring."

  let denote_arel rel args =
    failwith "TODO"

  let denote_arrinit vsz init s =
    match vsz.Value.datum with
    | VInt n ->
      let l  = Loc.fresh () in
      let c' = Runtime.EArrFill { loc = l ; idx = 0 ; init = init } in
      let s' = Map.add_exn ~key:l ~data:(Array.create ~len:n.value VUnit) s in
      Or_error.return (c', s')
    | _ ->
      let msg =
        Printf.sprintf "Type Error: Array of size %s."
          (value_to_string vsz)
      in
      Or_error.error_string msg

  let denote_arrread vloc vidx s =
    match vloc.Value.datum, vidx.Value.datum with
    | VLoc l, VInt n ->
      (match Map.find s l with
       | Some storev -> Or_error.return storev.(n.value)
       | None ->
         let msg = Printf.sprintf "Location %s not found in store %s."
             (Loc.to_string l)
             (store_to_string s)
         in
         Or_error.error_string msg)
    | _ ->
      let msg =
        Printf.sprintf "Type Error: Array at location %s indexed at %s."
          (value_to_string vloc)
          (value_to_string vidx)
      in
      Or_error.error_string msg

  let denote_arrwrite vloc vidx v s =
    match vloc.Value.datum, vidx.Value.datum with
    | VLoc l, VInt n ->
      (match Map.find s l with
       | Some storev ->
         let result = storev.(n.value) in
         storev.(n.value) <- v.Value.datum;
         Or_error.return result
       | None ->
         let msg = Printf.sprintf "Location %s not found in store %s."
             (Loc.to_string l)
             (store_to_string s)
         in
         Or_error.error_string msg)
    | _ ->
      let msg =
        Printf.sprintf "Type Error: Array at location %s indexed at %s being written value %s."
          (value_to_string vloc)
          (value_to_string vidx)
          (value_to_string v)
      in
      Or_error.error_string msg

  let denote_arrlen vloc s =
    match vloc.Value.datum with
    | VLoc l ->
      (match Map.find s l with
       | Some storev ->
         Or_error.return (VInt { value = Array.length storev ; label = Label.public ; region = Region.bottom })
       | None ->
         let msg = Printf.sprintf "Location %s not found in store %s."
             (Loc.to_string l)
             (store_to_string s)
         in
         Or_error.error_string msg)
    | _ ->
      let msg =
        Printf.sprintf "Type Error: Length of array at location %s."
          (value_to_string vloc)
      in
      Or_error.error_string msg

  let denote_cast x l e =
    match Map.find e x with
    | Some r ->
       let v = !r in
       (match v with
        | VFlip flip -> Or_error.return (VBool { value = flip.value ; label = l ; region = flip.region })
        | VRnd rnd   -> Or_error.return (VInt { value = rnd.value ; label = l ; region = rnd.region })
        | _ ->
          let msg =
            Printf.sprintf "Type Error: Cast of %s."
              (value_to_string' v)
          in
          Or_error.error_string msg)
    | None ->
      let msg =
        Printf.sprintf "Variable %s not found in environment %s."
          (Var.to_string x)
          (env_to_string e)
      in
      Or_error.error_string msg

  let denote_mux vg vlhs vrhs =
    match vg.Value.datum with
    | VBool b ->
      let (v2', v3') = if b.value then (vlhs, vrhs) else (vrhs, vlhs) in
      Or_error.return (VTuple (v2'.Value.datum, v3'.Value.datum))
    | _ ->
      let msg =
        Printf.sprintf "Type Error: Mux of %s."
          (value_to_string vg)
      in
      Or_error.error_string msg

  let merge m1 m2 =
    Map.merge
      ~f:(fun ~key:x v ->
          match v with
          | `Left v1 -> (Some v1)
          | `Right v2 -> (Some v2)
          | `Both (_, v2) -> (Some v2))
      m1 m2

  let rec bind p v =
    let open Or_error.Let_syntax in
    match p, v with
    | (Pattern.XWild, _) -> Or_error.return []
    | (Pattern.XVar x, _) -> Or_error.return [(x, ref v)]
    | (Pattern.XTuple (p1, p2), VTuple (v1, v2)) ->
      let%bind bs1 = bind p1 v1 in
      let%bind bs2 = bind p2 v2 in
      Or_error.return (bs1 @ bs2)
    | (Pattern.XRecord ps, VRecord fs) ->
      let check =
        List.fold2
          ps
          fs
          ~init:[]
          ~f:(fun acc (f1, p') (f2, v') ->
              if Var.equal f1 f2 then
                (bind p' v') :: acc
              else
                let msg =
                  Printf.sprintf "Binding Error: Record fields %s and %s do not match."
                    (Var.to_string f1)
                    (Var.to_string f2) in
                Or_error.error_string msg :: acc)
      in
      (match check with
       | Ok l ->
         let%bind l = Or_error.combine_errors l in
         Or_error.return (List.join l)
       | Unequal_lengths ->
         let msg =
           Printf.sprintf "Binding Error: Record pattern and value are different sizes."
         in
         Or_error.error_string msg)
    | (Pattern.XAscr (p', _), _) -> bind p' v
    | _ ->
      let msg =
        Printf.sprintf "Binding Error: Incompatible pattern, %s, and value, %s."
          (Pattern.to_string p)
          (value_to_string' v)
      in
      Or_error.error_string msg

  let bind_map p v =
    let open Or_error.Let_syntax in
    let%bind l = bind p v in
    Map.of_alist_or_error (module Var) l

  let denote_apply vlam varg e =
    let open Or_error.Let_syntax in
    match vlam.Value.datum with
    | VAbs f ->
      let%bind e' = bind_map f.pat varg.Value.datum in
      Or_error.return (f.body, merge f.env e')
    | _ ->
      let msg =
        Printf.sprintf "Type Error: Application of %s to %s."
          (value_to_string vlam)
          (value_to_string varg)
      in
      Or_error.error_string msg

  let denote_ite vguard thenb elseb =
    match vguard.Value.datum with
    | VBool b ->
      if b.value then
        Or_error.return thenb
      else
        Or_error.return elseb
    | _ ->
      let msg =
        Printf.sprintf "Type Error: Guard of conditional, %s, is not a boolean."
          (value_to_string vguard)
      in
      Or_error.error_string msg

  let denote_print v =
    Printf.printf "%s\n" (value_to_string v)

  let reduce c { e ; s } =
    let tag x = Or_error.tag ~tag:(Section.to_string c.source_location) x in
    let open Or_error.Let_syntax in
    match c.datum with
    | ELit l -> Or_error.return ({ c with datum = EVal (denote_lit l.datum l.label) }, { e ; s })
    | EFlip r -> Or_error.return ({ c with datum = EVal (denote_flip r) }, { e  ; s })
    | ERnd r -> Or_error.return ({ c with datum = EVal (denote_rnd r) }, { e ; s })
    | EVar (x :: acs) ->
      let r = denote_var x acs e in
      let%bind v = tag r in
      Or_error.return ({ c with datum = EVal v }, { e ; s })
    | EBOp bo ->
      let vargs = Runtime.to_values bo.args in
      let r = denote_bop bo.op vargs in
      let%bind v = tag r in
      Or_error.return ({ c with datum = EVal v }, { e ; s })
    | EAOp ao ->
      let vargs = Runtime.to_values ao.args in
      let r = denote_aop ao.op vargs in
      let%bind v = tag r in
      Or_error.return ({ c with datum = EVal v }, { e ; s })
    | EARel ar ->
      let vargs = Runtime.to_values ar.args in
      let r = denote_arel ar.rel vargs in
      let%bind v = tag r in
      Or_error.return ({ c with datum = EVal v }, { e ; s })
    | ETuple (l, r) ->
      let vl = Runtime.to_value l in
      let vr = Runtime.to_value r in
      Or_error.return ({ c with datum = EVal (VTuple (vl.datum, vr.datum)) }, { e ; s })
    | ERecord fields ->
      let vfields = List.map ~f:(fun (x, e) -> (x, (Runtime.to_value e).datum)) fields in
      Or_error.return ({ c with datum = EVal (VRecord vfields) }, { e ; s })
    | EArrInit ai ->
      let vsz = Runtime.to_value ai.size in
      let r = denote_arrinit vsz ai.init s in
      let%bind (c', s') = tag r in
      Or_error.return ({ c with datum = c' }, { e ; s = s' })
    | EArrFill af -> failwith "Come back after implementing application"
    | EArrRead ar ->
      let vloc, vidx = Runtime.to_value ar.loc, Runtime.to_value ar.idx in
      let r = denote_arrread vloc vidx s in
      let%bind v = tag r in
      Or_error.return ({ c with datum = EVal v }, { e ; s })
    | EArrWrite aw ->
      let vloc, vidx, v = Runtime.to_value aw.loc, Runtime.to_value aw.idx, Runtime.to_value aw.value in
      let r = denote_arrwrite vloc vidx v s in
      let%bind v = tag r in
      Or_error.return ({ c with datum = EVal v }, { e ; s })
    | EArrLen l ->
      let vloc = Runtime.to_value l in
      let r = denote_arrlen vloc s in
      let%bind v = tag r in
      Or_error.return ({ c with datum = EVal v }, { e ; s })
    | ECast cast ->
      let r = denote_cast cast.var cast.label e in
      let%bind v = tag r in
      Or_error.return ({ c with datum = EVal v }, { e ; s })
    | EMux m ->
      let vguard, vlhs, vrhs = Runtime.to_value m.guard, Runtime.to_value m.lhs, Runtime.to_value m.rhs in
      let r = denote_mux vguard vlhs vrhs in
      let%bind v = tag r in
      Or_error.return ({ c with datum = EVal v }, { e ; s })
    | EAbs f -> Or_error.return ({ c with datum = EVal (VAbs { env = e ; pat = f.pat ; body = f.body }) }, { e ; s })
    | ERec f ->
      let bogus = ref VUnit in
      let lam = VAbs { env = Map.set e ~key:f.name ~data:bogus ; pat = f.pat ; body = f.body } in
      bogus := lam;
      Or_error.return ({ c with datum = EVal lam }, { e ; s })
    | EApp ap ->
      let vlam, varg = Runtime.to_value ap.lam, Runtime.to_value ap.arg in
      let r = denote_apply vlam varg e in
      let%bind (c', e') = tag r in
      Or_error.return (c', { e = e' ; s })
    | ELet l ->
      let v = Runtime.to_value l.value in
      let%bind e' = tag (bind_map l.pat v.Value.datum) in
      Or_error.return (l.body, { e = merge e e' ; s })
    | EIf ite ->
      let vguard = Runtime.to_value ite.guard in
      let%bind c' = tag (denote_ite vguard ite.thenb ite.elseb) in
      Or_error.return (c', { e ; s })
    | EPrint t ->
      let v = Runtime.to_value t in
      (denote_print v;
       Or_error.return ({ c with datum = EVal VUnit }, { e ; s }))
    | EVar [] -> failwith "Impossible, forbidden by parser."
    | EVal _  -> failwith "Impossible, forbidden by evaluator."


end

include Eval.Make(Reduction)
