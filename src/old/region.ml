type t = Region of string

let to_string (Region x) = x

module Ordered : Map.OrderedType with type t = t =
  struct
    type var = t
    type t = var

    let compare (Region x) (Region y) = String.compare x y
  end

module Set = Extensions.Set.Make(Ordered)
module Map = Map.Make(Ordered)

let equal x y = Ordered.compare x y = 0

let min x y = if Ordered.compare x y <= 0 then x else y
let max x y = if Ordered.compare x y >= 0 then x else y
