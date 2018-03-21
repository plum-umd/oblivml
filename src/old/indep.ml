module RExp = Rexp

type t = Indep of RExp.t * RExp.t

module Ordered : Set.OrderedType with type t = t =
  struct
    type indep = t
    type t = indep

    let compare i1 i2 =
      let Indep (rexp11, rexp12) = i1 in
      let Indep (rexp21, rexp22) = i2 in
      let c1 = RExp.Ordered.compare rexp11 rexp21 in
      if c1 = 0 then
        RExp.Ordered.compare rexp12 rexp22
      else
        c1
  end

module Set = Extensions.Set.Make(Ordered)
module Map = Map.Make(Ordered)
  
let to_string i =
  match i with
  | Indep (rexp1, rexp2) -> Printf.sprintf
                              "%s _||_ %s"
                              (RExp.to_string rexp1)
                              (RExp.to_string rexp2)

let rsub i x s =
  match i with
  | Indep (rexp1, rexp2) -> Indep (RExp.rsub rexp1 x s, RExp.rsub rexp2 x s)
