type t = Var of string

let to_string (Var x) = x

module Ordered : Map.OrderedType with type t = t =
  struct
    type var = t
    type t = var

    let compare (Var x) (Var y) = String.compare x y
  end

module Set = Extensions.Set.Make(Ordered)
module Map = Map.Make(Ordered)
