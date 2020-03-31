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

  type env = (Var.t, value Ref.t, Var.comparator_witness) Map.t

  type store = (Loc.t, value Array.t, Loc.comparator_witness) Map.t

  type t = { e : env ; s : store }

  let empty = { e = Map.empty (module Var) ; s = Map.empty (module Loc) }

  let denote_lit d l =
    match d with
    | Literal.LitUnit () -> VUnit
    | Literal.LitBool b -> VBool { value = b ; label = l ; region = Region.bottom }
    | Literal.LitInt n -> VInt { value = n ; label = l ; region = Region.bottom }

  let denote_flip r = VFlip { value = (Random.bool ()) ; region = r }

  let denote_rnd r = VRnd { value = (Random.bits ()) ; region = r }

  let reduce c { e ; s } =
    match c.datum with
    | ELit l -> ({ c with datum = EVal (denote_lit l.datum l.label) }, { e ; s })
    | EFlip r -> ({ c with datum = EVal (denote_flip r) }, { e  ; s })
    | ERnd r -> ({ c with datum = EVal (denote_rnd r) }, { e ; s })
end

include Eval.Make(Reduction)

(*

let reduce c e s =
  match c.cdatum with
  | EFlip r -> ({ c with cdatum = EVal (VFlip { value = (Random.bool ()) ; region = r }) }, e, s)
  | ERnd r -> ({ c with cdatum = EVal (VRnd { value = (Random.bits ()) ; region = r }) }, e, s)
  | EVar path ->
    (match path with
     | [] -> failwith "Impossible, forbidden by lexer / parser."
     | x :: fields ->
       let mr = Map.find e x in
       (match mr with
        | Some r -> let v = !r in ({ c with cdatum = access c.csource_location v fields }, e, s)
        | None ->
          let msg =
            Printf.sprintf "Variable %s not found in environment %s."
              (Var.to_string x)
              (env_to_string e)
          in
          raise (RuntimeError (c.csource_location, msg))))
  | EBOp bo ->
    let vargs = exprs_to_values bo.args in
    ({ c with cdatum = EVal (denote_bo loc bo.op vargs) }, e, s)
  | EAOp ao ->
    let vargs = exprs_to_values ao.args in
    ({ c with cdatum = EVal (denote_ao loc ao.op vargs) }, e, s)
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
