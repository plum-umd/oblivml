open Core
open Stdio

module Base = struct
  module T = struct
    type t = Label of int
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make(T)
end

type t     = (Base.t, Base.comparator_witness) Set.t
let bottom = Set.empty (module Base)
let public = bottom
let top    = Set.singleton (module Base) (Base.Label 1)
let secret = top

let of_string s =
  match s with
  | "public" -> public
  | "secret" -> secret
  | _        -> failwith "Impossible: forbidden by lexer / parser"

let to_string l = if Set.equal l bottom then "public" else "secret"
let pp f l      = Format.pp_print_text f (to_string l)

let order l1 l2 = Set.is_subset l1 l2
let equal l1 l2 = (order l1 l2) && (order l2 l1)
let join  l1 l2 = Set.union l1 l2
