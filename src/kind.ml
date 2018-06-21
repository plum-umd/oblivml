open Core
open Stdio

type t =
  | Affine
  | Universal

let bottom = Affine
let top    = Universal

let to_string k =
  match k with
  | Universal -> "universal"
  | Affine    -> "affine"

let order k1 k2 =
  match (k1, k2) with
  | (Affine    , Affine)    -> true
  | (Affine    , Universal) -> true
  | (Universal , Affine)    -> false
  | (Universal , Universal) -> true

let join k1 k2 =
  match (k1, k2) with
  | (Affine    , Affine)    -> Affine
  | (Affine    , Universal) -> Universal
  | (Universal , Affine)    -> Universal
  | (Universal , Universal) -> Universal

let meet k1 k2 =
  match (k1, k2) with
  | (Affine    , Affine)    -> Affine
  | (Affine    , Universal) -> Affine
  | (Universal , Affine)    -> Affine
  | (Universal , Universal) -> Universal
