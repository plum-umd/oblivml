type t =
  | Bottom
  | Var of Region.t
  | Join of t * t

let bottom = Bottom

let rec to_string rexp =
  match rexp with
  | Bottom -> "_|_"
  | Var x  -> Region.to_string x
  | Join (rexp1, rexp2) -> Printf.sprintf "%s join %s" (to_string rexp1) (to_string rexp2)

module Ordered : Set.OrderedType with type t = t =
  struct
    type rexp = t
    type t = rexp

    let compare rexp1 rexp2 =
      match (rexp1, rexp2) with
      | (Bottom, Bottom) -> 0
      | (Bottom, Var _) -> -1
      | (Bottom, Join (_, _)) -> -1
      | (Var _, Bottom) -> 1
      | (Var r1, Var r2) -> Region.Ordered.compare r1 r2
      | (Var _, Join (_, _)) -> -1
      | (Join (_, _), Bottom) -> 1
      | (Join (_, _), Var _) -> 1
      | (Join (rexp11, rexp12), Join (rexp21, rexp22)) ->
         let c1 = compare rexp11 rexp21 in
         if c1 = 0 then
           compare rexp12 rexp22
         else
           c1
  end

let equal rexp1 rexp2 = Ordered.compare rexp1 rexp2 = 0

let rec free rexp =
  match rexp with
  | Bottom -> Region.Set.empty
  | Var r -> Region.Set.singleton r
  | Join (rexp1, rexp2) -> Region.Set.union (free rexp1) (free rexp2)

let rec rsub r x s =
  match r with
  | Bottom -> Bottom
  | Var x' -> if Region.equal x x' then s else Var x'
  | Join (r1, r2) -> Join (rsub r1 x s, rsub r2 x s)

