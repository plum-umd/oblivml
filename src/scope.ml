include Region.Set

module RExp = Rexp
      
let to_string scope = Printf.sprintf "Ξ [ %s ]" (String.concat ", " (List.map Region.to_string (elements scope)))

let fresh rs =
  let rec fresh' n =
    let cand = Region.Region ("fresh" ^ (string_of_int n)) in
    if not (mem cand rs) then
      cand
    else
      fresh' (n + 1)
  in
  fresh' 0

let rec deduce_rexp scope rexp =
  match rexp with
  | RExp.Bottom -> true
  | RExp.Var x -> mem x scope
  | RExp.Join (rexp1, rexp2) -> (deduce_rexp scope rexp1) && (deduce_rexp scope rexp2)

let rec deduce_indep scope i =
  match i with
  | Indep.Indep (rexp1, rexp2) -> (deduce_rexp scope rexp1) && (deduce_rexp scope rexp2)
                              
let rec deduce_constrs scope cs =
  Constraints.for_all (deduce_indep scope) cs
  
let rec deduce_type scope t =
  match t with
  | Type.TUnit -> true
  | Type.TBit (_, r) -> deduce_rexp scope r
  | Type.TFlip r -> deduce_rexp scope r
  | Type.TInt (_, r) -> deduce_rexp scope r
  | Type.TAlias _ -> failwith "Should never deduce type which is not fully resolved"
  | Type.TTuple (t1, t2) -> (deduce_type scope t1) && (deduce_type scope t2)
  | Type.TExistential (rs, cs, t') ->
     let rs_set = Region.Set.of_list rs in
     if disjoint scope rs_set then
       let scope' = union scope rs_set in
       (deduce_constrs scope' cs) && (deduce_type scope' t')
     else
       false
  | Type.TRecord fields -> Field.Map.for_all (fun _ t -> deduce_type scope t) fields
  | Type.TArray ele_t -> deduce_type scope ele_t
  | Type.TFun (t1, t2) -> (deduce_type scope t1) && (deduce_type scope t2)
  
