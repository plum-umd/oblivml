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
    | _ -> failwith "Unimplemented"

  let value_to_string v = Value.to_string v value_to_string'

  type env = (Var.t, value Ref.t, Var.comparator_witness) Map.t

  let env_to_string e = "{ TODO }"

  type store = (Loc.t, value Array.t, Loc.comparator_witness) Map.t

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

  let reduce c { e ; s } =
    let tag = Or_error.tag ~tag:(Section.to_string c.source_location) in
    match c.datum with
    | ELit l -> Or_error.return ({ c with datum = EVal (denote_lit l.datum l.label) }, { e ; s })
    | EFlip r -> Or_error.return ({ c with datum = EVal (denote_flip r) }, { e  ; s })
    | ERnd r -> Or_error.return ({ c with datum = EVal (denote_rnd r) }, { e ; s })
    | EVar (x :: acs) ->
      let open Or_error.Let_syntax in
      let r = denote_var x acs e in
      let%bind v = tag r in
      Or_error.return ({ c with datum = EVal v }, { e ; s })
    | EBOp bo ->
      let vargs = Runtime.to_values bo.args in
      let open Or_error.Let_syntax in
      let r = denote_bop bo.op vargs in
      let%bind v = tag r in
      Or_error.return ({ c with datum = EVal v }, { e ; s })
    | EAOp ao ->
      let vargs = Runtime.to_values ao.args in
      let open Or_error.Let_syntax in
      let r = denote_aop ao.op vargs in
      let%bind v = tag r in
      Or_error.return ({ c with datum = EVal v }, { e ; s })

end

include Eval.Make(Reduction)

(*

let reduce c e s =
  match c.cdatum with
  | EARel ar ->
    let vargs = exprs_to_values ar.args in
    ({ c with cdatum = EVal (denote_ar loc ar.rel vargs) }, e, s)
  | ETuple (l, r) ->
    let vl = expr_to_value l in
    let vr = expr_to_value r in
    ({ c with cdatum = EVal (VTuple (vl, vr)) }, e, s)
  | ERecord fields ->
    let vfields = List.map ~f:(fun (x, e) -> (x, expr_to_value e)) fields in
    ({ c with cdatum = EVal (VRecord vfields) }, e, s)
  | EArrInit ai ->
    let vsz, vinit = expr_to_value ai.size, expr_to_value ai.init in
    (match vsz.vdatum, vinit.vdatum with
     | VInt n, VAbs f -> failwith "TODO"
       (* hmm *)
     | _ ->
       let msg =
         Printf.sprintf "Type Error: Array of size %s initialized by %s."
           (value_to_string vsz)
           (value_to_string vinit)
       in
       raise (RuntimeError (c.csource_location, msg)))
  | EArrRead ar ->
    let vloc, vidx = expr_to_value ar.loc, expr_to_value ar.idx in
    (match vloc.vdatum, vidx.vdatum with
     | VLoc l, VInt n ->
       (match Map.find s l with
        | Some storev -> ({ c with cdatum = EVal storev.(n.value) }, e, s)
        | None ->
          let msg = Printf.sprintf "Location %s not found in store %s."
              (Loc.to_string l)
              (store_to_string s)
          in
          raise (RuntimeError (c.csource_location, msg)))
     | _ ->
       let msg =
         Printf.sprintf "Type Error: Array at location %s indexed at %s."
           (value_to_string vloc)
           (value_to_string vidx)
       in
       raise (RuntimeError (c.csource_location, msg)))
  | EArrWrite aw ->
    let vloc, vidx, v = expr_to_value aw.loc, expr_to_value aw.idx, expr_to_value aw.value in
    (match vloc.vdatum, vidx.vdatum with
     | VLoc l, VInt n ->
       (match Map.find s l with
        | Some storev ->
          let result = storev.(n.value) in
          storev.(n.value) <- v.vdatum; ({ c with cdatum = EVal result }, e, s)
        | None ->
          let msg = Printf.sprintf "Location %s not found in store %s."
              (Loc.to_string l)
              (store_to_string s)
          in
          raise (RuntimeError (c.csource_location, msg)))
     | _ ->
       let msg =
         Printf.sprintf "Type Error: Array at location %s indexed at %s being written value %s."
           (value_to_string vloc)
           (value_to_string vidx)
           (value_to_string v)
       in
       raise (RuntimeError (c.csource_location, msg)))
  | EArrLen l ->
    let vloc = expr_to_value l in
    (match vloc.vdatum with
     | VLoc l ->
       (match Map.find s l with
        | Some storev -> ({ c with cdatum = EVal (VInt { value = Array.length storev ; label = Label.public ; region = Region.bottom }) }, e, s)
        | None ->
          let msg = Printf.sprintf "Location %s not found in store %s."
              (Loc.to_string l)
              (store_to_string s)
          in
          raise (RuntimeError (c.csource_location, msg)))
     | _ ->
       let msg =
         Printf.sprintf "Type Error: Length of array at location %s."
           (value_to_string vloc)
       in
       raise (RuntimeError (c.csource_location, msg)))
  | ECast cast ->
    (match Map.find e cast.var with
     | Some r ->
       let v = !r in
       let v' =
         (match v with
          | VFlip flip -> VBool { value = flip.value ; label = cast.label ; region = flip.region }
          | VRnd rnd -> VInt { value = rnd.value ; label = cast.label ; region = rnd.region }
          | _ ->
            let msg =
              Printf.sprintf "Type Error: Cast of %s."
                (value_to_string v)
            in
            raise (RuntimeError (c.csource_location, msg)))
       in
       ({ c with cdatum = EVal v' }, e, s)
     | None ->
       let msg =
         Printf.sprintf "Variable %s not found in environment %s."
           (Var.to_string cast.var)
           (env_to_string e)
       in
       raise (RuntimeError (c.csource_location, msg)))

  | EMux m ->
    let vguard, vlhs, vrhs = expr_to_value m.guard, expr_to_value m.lhs, expr_to_value m.rhs in
    (match vguard.vdatum with
     | VBool b ->
       let (v2', v3') = if b.value then (vlhs, vrhs) else (vrhs, vlhs) in
       ({ c with cdatum = EVal (VTuple (v2', v3')) }, e, s)
     | _ ->
       let msg =
         Printf.sprintf "Type Error: Mux of %s."
           (value_to_string vguard)
       in
       raise (RuntimeError (c.csource_location, msg)))
  | EAbs f -> ({ c with cdatum = EVal (VAbs { env = e ; pat = f.pat ; body = f.body }) }, e, s)
  | ERec f ->
    let bogus = ref VUnit in
    let lam = VAbs { env = Map.set e ~key:f.name ~data:bogus ; pat = f.pat ; body = f.body } in
    bogus := lam;
    ({ c with cdatum = EVal lam }, e, s)
  | EApp ap ->
    let vlam, varg = expr_to_value ap.lam, expr_to_value ap.arg in
    (match vlam.vdatum with
     | VAbs f ->
       let e' = apply f.env f.pat varg in
       (f.body, e', s)
     | _ ->
       let msg =
         Printf.sprintf "Type Error: Application of %s to %s."
           (value_to_string vlam)
           (value_to_string varg)
       in
       raise (RuntimeError (c.csource_location, msg)))
  | ELet l ->
    let v = expr_to_value l.value in
    let e' = apply e l.pat v in
    (l.body, e', s)
  | EIf ite ->
    let vguard = expr_to_value ite.guard in
    (match vguard.vdatum with
     | VBool b ->
       if b.value then
         (ite.thenb, e, s)
       else
         (ite.elseb, e, s)
     | _ ->
       let msg =
         Printf.sprintf "Type Error: If %s then %s else %s."
           (value_to_string vguard)
           (expr_to_string ite.thenb)
           (expr_to_string ite.elseb)
       in
       raise (RuntimeError (c.csource_location, msg)))
  | EPrint r ->
    let v = expr_to_value r in
    Printf.printf "%s\n" (value_to_string v);
    ({ c with cdatum = EVal VUnit }, e, s)
*)
