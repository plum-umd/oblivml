include Indep.Set

module RExp = Rexp

let to_string cs = Printf.sprintf "{ %s }" (String.concat ", " (List.map Indep.to_string (elements cs)))
            
let rec normalize_indep i =
  match i with
  | Indep.Indep (RExp.Var a1, RExp.Var a2) ->
     Indep.Set.singleton (Indep.Indep (RExp.Var (Region.min a1 a2), RExp.Var (Region.max a1 a2)))
  | Indep.Indep (RExp.Bottom, _) -> Indep.Set.empty
  | Indep.Indep (_, RExp.Bottom) -> Indep.Set.empty
  | Indep.Indep (RExp.Var a, RExp.Join (r1, r2)) -> Indep.Set.union (normalize_indep (Indep.Indep (RExp.Var a, r1))) (normalize_indep (Indep.Indep (RExp.Var a, r2)))
  | Indep.Indep (RExp.Join (r1, r2), r3) -> Indep.Set.union (normalize_indep (Indep.Indep (r1, r3))) (normalize_indep (Indep.Indep (r2, r3)))

let normalize cs = Indep.Set.fold (fun i acc -> Indep.Set.union (normalize_indep i) acc) cs Indep.Set.empty
                 
let rec deduce cs i =
  let i_normal = normalize_indep i in
  let cs_normal = normalize cs in
  Indep.Set.subset i_normal cs_normal

let deduces cs cs' =
  for_all (fun d -> deduce cs d) cs'

let rsub1 cs x s =
  map (fun c -> Indep.rsub c x s) cs
  
let rsub cs xs ss =
  List.fold_left2 rsub1 cs xs ss
