open Core
open Stdio

module T = struct
  type t = Loc of Int.t
  [@@deriving compare, sexp_of]

  let equal (Loc x) (Loc y) = Int.equal x y

  let to_string (Loc x) = Int.to_string x
end

include T
include Comparator.Make(T)

let curr_loc = ref 0

let fresh () =
  let ret = !curr_loc in
  curr_loc := ret + 1;
  Loc ret
