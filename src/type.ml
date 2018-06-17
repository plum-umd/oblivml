open Core
open Stdio

module Base =
  struct
    type t =
      | TBUnit
      | TBBool
      | TBInt
      | TBRBool
      | TBRInt

    let of_string s =
      match s with
      | "unit"  -> TBUnit
      | "bool"  -> TBBool
      | "int"   -> TBInt
      | "rbool" -> TBRBool
      | "rint"  -> TBRInt
      | _       -> failwith "Impossible: forbidden by lexer / parser."

    let to_string tb =
      match tb with
      | TBUnit -> "unit"
      | TBBool -> "bool"
      | TBInt  -> "int"
      | TBRBool -> "rbool"
      | TBRInt -> "rint"

    let accessible tb =
      match tb with
      | TBUnit -> Kind.Universal
      | TBBool -> Kind.Universal
      | TBInt  -> Kind.Universal
      | TBRBool -> Kind.Affine
      | TBRInt -> Kind.Affine
  end

type t =
  | TBase   of Base.t * Label.t * Region.Expr.t
  | TAlias  of Var.t
  | TTuple  of t * t
  | TRecord of (Var.t, t Option.t, Var.comparator_witness) Map.t
  | TArray  of t
  | TFun    of t * t

let rec to_string t =
  match t with
  | TBase (tb, l, r) -> Printf.sprintf "%s<%s, %s>" (Base.to_string tb) (Label.to_string l) (Region.Expr.to_string r)
  | TAlias ta -> Var.to_string ta
  | TTuple (t1, t2) -> Printf.sprintf "(%s * %s)" (to_string t1) (to_string t2)
  | TArray t -> Printf.sprintf "%s array" (to_string t)
  | TFun (t1, t2) -> Printf.sprintf "%s -> %s" (to_string t1) (to_string t2)
  | _ -> ""

let rec accessible t =
  match t with
  | TBase (tb, _, _) -> Base.accessible tb
  | TAlias _ -> Kind.Universal
  | TTuple (t1, t2) -> Kind.join (accessible t1) (accessible t2)
  | TRecord bs ->
     Map.fold bs ~init:Kind.bottom ~f:(fun ~key:_ ~data:m_t k -> Kind.join (match m_t with | None -> Kind.Universal | Some t -> accessible t) k)
  | TArray _ -> Kind.Universal
  | TFun _ -> Kind.Universal

(*
let rec to_string t =
  match t with
  | TUnit -> Printf.sprintf "unit"
  | TBit (l, r) -> Printf.sprintf "bit@(%s, %s)" (Label.to_string l) (Region.Expr.to_string r)
  | TFlip r -> Printf.sprintf "flip@(%s)" (Region.Expr.to_string r)
  | TInt (l, r) -> Printf.sprintf "int@(%s, %s)" (Label.to_string l) (Region.Expr.to_string r)
  | TAlias name        -> Printf.sprintf "%s" (Var.to_string name)
  | TTuple  (t1, t2)   -> Printf.sprintf "(%s ⨯ %s)" (to_string t1) (to_string t2)
  | TExistential (rs, cs, t) -> Printf.sprintf "∃ %s . %s & %s"
                                               (String.concat ", "
                                                              (List.map Region.to_string rs))
                                               (Constraints.to_string cs)
                                               (to_string t)
  | TRecord fields     -> Printf.sprintf "{ %s }" (String.concat ", "
                                                                 (List.map (fun (field, t) ->
                                                                      Printf.sprintf "%s : %s" (Field.to_string field) (to_string t))
                                                                           (Field.Map.bindings fields)))
  | TArray t           -> Printf.sprintf "%s array" (to_string t)
  | TFun (t1, t2) -> Printf.sprintf "%s -> %s" (to_string t1) (to_string t2)

let rec equal t1 t2 =
  match (t1, t2) with
  | (TUnit, TUnit) ->
     true
  | (TBit (l1, r1), TBit (l2, r2)) ->
     Label.equal l1 l2 && Region.Expr.equal r1 r2
  | (TFlip r1, TFlip r2) ->
     Region.Expr.equal r1 r2
  | (TInt (l1, r1), TInt (l2, r2)) ->
     Label.equal l1 l2 && Region.Expr.equal r1 r2
  | (TTuple (t11, t12), TTuple (t21, t22)) ->
     (equal t11 t21) && (equal t12 t22)
  | (TExistential (rs1, cs1, t1), TExistential (rs2, cs2, t2)) ->
     failwith "equality for existentials unimplemented"
  | (TRecord fields1, TRecord fields2) ->
     Field.Map.equal equal fields1 fields2
  | (TArray t1', TArray t2') ->
     equal t1' t2'
  | (TFun (t11, t12), TFun (t21, t22)) ->
     (equal t11 t21) && (equal t12 t22)
  | _ -> false

let rec free t =
  match t with
  | TUnit -> Region.Set.empty
  | TBit (_, r) -> Region.Expr.free r
  | TFlip r -> Region.Expr.free r
  | TInt (_, r) -> Region.Expr.free r
  | TAlias name -> failwith (Printf.sprintf "free should only be called on fully-resolved types: %s" (to_string t))
  | TTuple (t1, t2) -> Region.Set.union (free t1) (free t2)
  | TExistential (rs, _, t') -> Region.Set.diff (free t') (Region.Set.of_list rs)
  | TRecord fields -> Field.Map.fold (fun _ value acc -> Region.Set.union acc (free value)) fields Region.Set.empty
  | TArray ele_t -> free ele_t
  | TFun (t1, t2) -> Region.Set.union (free t1) (free t2)

let fresh x s t =
  let prefix = "interior" in
  let rec fresh' n =
    let curr = Region.Region (prefix ^ (string_of_int n)) in
    if not (Region.equal x curr) && not (Region.Set.mem curr (Region.Expr.free s)) && not (Region.Set.mem curr (free t)) then
      curr
    else
      fresh' (n + 1)
  in
  fresh' 0

let rec rsub1 t x s =
  match t with
  | TUnit -> TUnit
  | TBit (l, r) -> TBit (l, Region.Expr.rsub r x s)
  | TFlip r -> TFlip (Region.Expr.rsub r x s)
  | TInt (l, r) -> TInt (l, Region.Expr.rsub r x s)
  | TAlias x -> failwith (Printf.sprintf "types should be fully resolved for substitution: %s" (to_string t))
  | TTuple (t1, t2) -> TTuple (rsub1 t1 x s, rsub1 t2 x s)
  | TExistential (ys, cs, t_body) ->
     let (ys_ret, t_body_ret) = List.fold_right
                             (fun y (ys', t_body') ->
                               let (z, t_body'') = capture_avoid_sub y t_body' x s in
                               let ys'' = z :: ys' in
                               (ys'', t_body''))
                             ys
                             ([], t_body)
     in
     TExistential (ys_ret, cs, t_body_ret)
  | TRecord fields -> TRecord (Field.Map.map (fun field_type -> rsub1 field_type x s) fields)
  | TArray ele_t -> TArray (rsub1 ele_t x s)
  | TFun (t1, t2) -> TFun (rsub1 t1 x s, rsub1 t2 x s)

and capture_avoid_sub y t x s =
  if Region.equal x y then
    (y, t)
  else
    if not (Region.Set.mem y (Region.Expr.free s)) then
      (y, rsub1 t x s)
    else
      let z = fresh x s t in
      (z, rsub1 (rsub1 t y (Region.Expr.Var z)) x s)

let rsub t xs ss =
  List.fold_left2 rsub1 t xs ss

let rec refine t rs_set = t
                        (*
  match t with
  | TUnit -> TUnit
  | TBit (l, r) -> TBit (l, r)
  | TFlip r -> TFlip r
  | TInt (l, r) -> TInt (l, r)
  | TTuple (t1, t2) -> TTuple (refine t1 rs_set, refine t2 rs_set)
  | TRecord fields -> TRecord (Field.Map.map (fun field_type -> refine field_type rs_set) fields)
  | TArray ele_t -> TArray (refine ele_t rs_set)
  | TFun (t1, t2) -> TFun (refine t1 rs_set, refine t2 rs_set)
                         *)
let rec merge t1 t2 =
  match (t1, t2) with
  | (TUnit, TUnit) -> TUnit
  | (TBit (l1, r1), TBit (l2, r2)) ->
     let l' = Label.join l1 l2 in
     let r' = Region.Expr.Join (r1, r2) in
     TBit (l', r')
  | (TFlip r1, TFlip r2) ->
     let r' = Region.Expr.Join (r1, r2) in
     TFlip r'
  | (TInt (l1, r1), TInt (l2, r2)) ->
     let l' = Label.join l1 l2 in
     let r' = Region.Expr.Join (r1, r2) in
     TInt (l', r')
  | (TTuple (t11, t12), TTuple (t21, t22)) ->
     let t_left = merge t11 t21 in
     let t_right = merge t12 t22 in
     TTuple (t_left, t_right)
  | (TRecord fields1, TRecord fields2) ->
     let fields' = Field.Map.merge
                     (fun name t1 t2 ->
                       match (t1, t2) with
                       | (Some t1', Some t2') -> Some (merge t1' t2')
                       | _ -> failwith "Type.merge on records with different fields")
                     fields1
                     fields2
     in
     TRecord fields'
  | _ -> let err = Printf.sprintf
                     "Type.merge on bad types (or unmatching types) %s and %s."
                     (to_string t1)
                     (to_string t2)
         in
         failwith err

let rec join l r t =
  match t with
  | TUnit -> TUnit
  | TBit (l', r') -> TBit (Label.join l l', Region.Expr.Join (r, r'))
  | TFlip r' -> TFlip (Region.Expr.Join (r, r'))
  | TInt (l', r') -> TInt (Label.join l l', Region.Expr.Join (r, r'))
  | TAlias x -> TAlias x
  | TTuple (t1, t2) -> TTuple (join l r t1, join l r t2)
  | TRecord fields -> TRecord (Field.Map.map (join l r) fields)
  | _ -> let err = Printf.sprintf
                     "Type.join on bad type %s."
                     (to_string t)
         in
         failwith err
 *)
