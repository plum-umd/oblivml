open Error

type base = Label of int

module OrderedBase =
  struct
    type t = base

    let compare (Label b1) (Label b2) = compare b1 b2
  end

module BaseSet = Set.Make(OrderedBase)

type t     = BaseSet.t
let bottom = BaseSet.empty
let public = bottom
let top = BaseSet.singleton (Label 1)
let secret = top

let of_string s =
  match s with
  | "public" -> public
  | "secret" -> secret
  | _        -> raise Impossible (* Forbidden by lexer / parser *)

let to_string l = if BaseSet.equal l bottom then "public" else "secret"
let pp f l = Format.pp_print_text f (to_string l)

let order l1 l2 = BaseSet.subset l1 l2
let equal l1 l2 = (order l1 l2) && (order l2 l1)
let join  l1 l2 = BaseSet.union  l1 l2
