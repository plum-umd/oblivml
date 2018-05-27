open Core
open Stdio

module T = struct
  type t = Var of String.t
  [@@deriving compare, sexp_of]

  let to_string (Var x) = x
end

include T
include Comparator.Make(T)
