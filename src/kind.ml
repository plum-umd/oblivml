type t     = U | A
let bottom = A
let top    = U

let to_string k =
  match k with
    | U -> "universal"
    | A -> "affine"

let order k1 k2 = 
  match (k1, k2) with
    | (A, A) -> true
    | (A, U) -> true
    | (U, A) -> false
    | (U, U) -> true

let join k1 k2 = 
  match (k1, k2) with
    | (A, A) -> A
    | (A, U) -> U
    | (U, A) -> U
    | (U, U) -> U

let meet k1 k2 =
  match (k1, k2) with
    | (A, A) -> A
    | (A, U) -> A
    | (U, A) -> A
    | (U, U) -> U
