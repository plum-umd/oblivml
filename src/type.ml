open Core
open Stdio

module Base =
struct
  type t =
    | TBUnit
    | TBBool
    | TBInt
    | TBRBool
    | TBRInt

  let of_string s =
    match s with
    | "unit"  -> TBUnit
    | "bool"  -> TBBool
    | "int"   -> TBInt
    | "rbool" -> TBRBool
    | "rint"  -> TBRInt
    | _       -> failwith "Impossible: forbidden by lexer / parser."

  let to_string tb =
    match tb with
    | TBUnit  -> "unit"
    | TBBool  -> "bool"
    | TBInt   -> "int"
    | TBRBool -> "rbool"
    | TBRInt  -> "rint"

  let accessible tb =
    match tb with
    | TBUnit  -> Kind.Universal
    | TBBool  -> Kind.Universal
    | TBInt   -> Kind.Universal
    | TBRBool -> Kind.Affine
    | TBRInt  -> Kind.Affine
end

type t =
  | TBase   of Base.t * Label.t * Region.t
  | TAlias  of Var.t
  | TTuple  of t * t
  | TRecord of (Var.t, t Option.t, Var.comparator_witness) Map.t
  | TArray  of t
  | TFun    of t * t

let rec to_string t =
  match t with
  | TBase (tb, l, r) -> Printf.sprintf "%s<%s, %s>" (Base.to_string tb) (Label.to_string l) (Region.to_string r)
  | TAlias ta        -> Var.to_string ta
  | TTuple (t1, t2)  -> Printf.sprintf "(%s * %s)" (to_string t1) (to_string t2)
  | TArray t         -> Printf.sprintf "%s array" (to_string t)
  | TFun (t1, t2)    -> Printf.sprintf "%s -> %s" (to_string t1) (to_string t2)
  | _                -> ""

let rec accessible t =
  match t with
  | TBase (tb, _, _) -> Base.accessible tb
  | TAlias _         -> Kind.Universal
  | TTuple (t1, t2)  -> Kind.join (accessible t1) (accessible t2)
  | TRecord bs       -> Map.fold
                          bs
                          ~init:Kind.bottom
                          ~f:(fun ~key:_ ~data:m_t k ->
                              Kind.join
                                (match m_t with
                                 | None -> Kind.Universal
                                 | Some t -> accessible t)
                                k)
  | TArray _         -> Kind.Universal
  | TFun _           -> Kind.Universal

(* TODO(ins): FIXME *)
let rec equal t1 t2 = failwith "Unimplemented"

(* TODO(ins): FIXME *)
let rec subst x t e = failwith "Unimplemented"
