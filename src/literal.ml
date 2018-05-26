open Core
open Stdio

type t =
  | LitUnit of Unit.t
  | LitBool of Bool.t
  | LitInt  of Int.t

let to_type (l : t) : Type.Base.t =
  match l with
  | LitUnit _ -> Type.Base.TBUnit
  | LitBool _ -> Type.Base.TBBool
  | LitInt  _ -> Type.Base.TBInt
