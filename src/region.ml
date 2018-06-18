open Core
open Stdio

type t = (Var.t, Var.comparator_witness) Set.t

let bottom = Set.empty (module Var)

let var v = Set.singleton (module Var) v

let join r1 r2 = Set.union r1 r2

let equiv r1 r2 = Set.equal r1 r2

let to_string r = Printf.sprintf "%s \/ _|_" (String.concat (List.map (Set.to_list r) ~f:Var.to_string) ~sep:" \/ ")
