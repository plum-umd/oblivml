open Core

(** Runtime expressions with source location information.
    Parameterized by value type. *)
type 'v t = { source_location : Section.t
            ; datum : 'v t' }

and 'v t' =
  | ELit      of { datum : Literal.t ; label : Label.t }
  | EVal      of 'v
  | EFlip     of Region.t
  | ERnd      of Region.t
  | EVar      of Var.t List.t
  | EBOp      of { op : Boolean.Op.t ; args : 'v t List.t }
  | EAOp      of { op : Arith.Op.t ; args : 'v t List.t }
  | EARel     of { rel : Arith.Rel.t ; args : 'v t List.t }
  | ETuple    of ('v t, 'v t) Tuple.T2.t
  | ERecord   of (Var.t * 'v t) List.t
  | EArrInit  of { size : 'v t ; init : 'v t }
  | EArrFill  of { size : 'v Value.t ; init : 'v t ; acc : ('v Value.t) List.t ; curr : 'v t }
  | EArrRead  of { loc : 'v t ; idx : 'v t }
  | EArrWrite of { loc : 'v t ; idx : 'v t ; value : 'v t }
  | EArrLen   of 'v t
  | ECast     of { var : Var.t ; label : Label.t }
  | EMux      of { guard : 'v t ; lhs : 'v t ; rhs : 'v t }
  | EAbs      of { pat : Pattern.t ; body : 'v t }
  | ERec      of { name : Var.t ; pat : Pattern.t ; body : 'v t }
  | EApp      of { lam : 'v t ; arg : 'v t }
  | ELet      of { pat : Pattern.t ; value : 'v t ; body : 'v t }
  | EIf       of { guard : 'v t ; thenb : 'v t ; elseb : 'v t }
  | EPrint    of 'v t

let is_value_datum (c : 'v t') : Bool.t =
  match c with
  | EVal _ -> true
  | _      -> false

let is_value (c : 'v t) : Bool.t = is_value_datum c.datum

let is_redex_datum (c : 'v t') : Bool.t =
  match c with
  | ELit  _         -> true
  | EVal  _         -> false
  | EFlip _         -> true
  | ERnd  _         -> true
  | EVar  _         -> true
  | EBOp bo         -> List.for_all bo.args ~f:is_value
  | EAOp ao         -> List.for_all ao.args ~f:is_value
  | EARel ar        -> List.for_all ar.args ~f:is_value
  | ETuple (e1, e2) -> is_value e1 && is_value e2
  | ERecord fields  -> List.for_all fields ~f:(Fn.compose is_value Tuple.T2.get2)
  | EArrInit ai     -> is_value ai.size && is_value ai.init
  | EArrFill af     -> is_value af.curr
  | EArrRead ar     -> is_value ar.loc && is_value ar.idx
  | EArrWrite aw    -> is_value aw.loc && is_value aw.idx && is_value aw.value
  | EArrLen loc     -> is_value loc
  | ECast _         -> true
  | EMux m          -> is_value m.guard && is_value m.lhs && is_value m.rhs
  | EAbs _          -> true
  | ERec _          -> true
  | EApp ap         -> is_value ap.lam && is_value ap.arg
  | ELet l          -> is_value l.value
  | EIf ite         -> is_value ite.guard
  | EPrint e        -> is_value e

(* Another possibility for this is to define a datatype of reducible
   expressions which contain values. For example:

   type 'v redex =
     ...
     | EArrRead of { size : 'v ; init : 'v }
     ...

   type 'v t' =
     | Prim of 'v redex *)

let is_redex (c : 'v t) : Bool.t = is_redex_datum c.datum

let rec of_source_datum (s : Source.t') : 'v t' =
  match s with
  | ELit l          -> ELit { datum = l.datum ; label = l.label }
  | EFlip r         -> EFlip r
  | ERnd r          -> ERnd r
  | EVar p          -> EVar p
  | EBOp bo         -> EBOp { op = bo.op ; args = List.map ~f:of_source bo.args }
  | EAOp ao         -> EAOp { op = ao.op ; args = List.map ~f:of_source ao.args }
  | EARel ar        -> EARel { rel = ar.rel ; args = List.map ~f:of_source ar.args }
  | ETuple (e1, e2) -> ETuple (of_source e1, of_source e2)
  | ERecord fields  -> ERecord (List.map ~f:(fun (x, e) -> (x, of_source e)) fields)
  | EArrInit ai     -> EArrInit { size = of_source ai.size ; init = of_source ai.init }
  | EArrRead ar     -> EArrRead { loc = of_source ar.loc ; idx = of_source ar.idx }
  | EArrWrite aw    -> EArrWrite { loc = of_source aw.loc ; idx = of_source aw.idx ; value = of_source aw.value }
  | EArrLen loc     -> EArrLen (of_source loc)
  | ECast c         -> ECast { var = c.var ; label = c.label }
  | EMux m          -> EMux { guard = of_source m.guard ; lhs = of_source m.lhs ; rhs = of_source m.rhs }
  | EAbs lam        -> EAbs { pat = lam.pat ; body = of_source lam.body }
  | ERec rlam       -> ERec { name = rlam.name ; pat = rlam.pat ; body = of_source rlam.body }
  | EApp ap         -> EApp { lam = of_source ap.lam ; arg = of_source ap.arg }
  | ELet l          -> ELet { pat = l.pat ; value = of_source l.value ; body = of_source l.body }
  | EType ty        -> (of_source ty.body).datum
  | EIf ite         -> EIf { guard = of_source ite.guard ; thenb = of_source ite.thenb ; elseb = of_source ite.elseb }
  | EPrint s        -> EPrint (of_source s)

and of_source s = { source_location = s.source_location ; datum = of_source_datum s.datum }

let of_value v = { source_location = v.Value.source_location ; datum = EVal v.Value.datum }

let of_values vs = List.map ~f:of_value vs

let to_value e =
  match e.datum with
  | EVal v -> { Value.source_location = e.source_location ; Value.datum = v }
  | _ -> failwith "Impossible"

let to_values es = List.map ~f:to_value es
