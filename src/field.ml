type t = Field of string

let to_string (Field a) = a

module Ordered : Map.OrderedType with type t = t =
  struct
    type alias = t
    type t = alias

    let compare (Field a) (Field b) = String.compare a b
  end

module Set = Extensions.Set.Make(Ordered)
module Map = Map.Make(Ordered)
