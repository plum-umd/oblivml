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
    | TBNUBool
    | TBNUInt

  let equal tb1 tb2 = tb1 = tb2

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
    | TBUnit   -> "unit"
    | TBBool   -> "bool"
    | TBInt    -> "int"
    | TBRBool  -> "rbool"
    | TBRInt   -> "rint"
    | TBNUBool -> "nubool"
    | TBNUInt  -> "nuint"

  let accessible tb =
    match tb with
    | TBUnit   -> Kind.Universal
    | TBBool   -> Kind.Universal
    | TBInt    -> Kind.Universal
    | TBRBool  -> Kind.Affine
    | TBRInt   -> Kind.Affine
    | TBNUBool -> Kind.Affine
    | TBNUInt  -> Kind.Affine

  let safe tb =
    match tb with
    | TBUnit   -> true
    | TBBool   -> true
    | TBInt    -> true
    | TBRBool  -> true
    | TBRInt   -> true
    | TBNUBool -> false
    | TBNUInt  -> false
end

type t =
  | TBase   of Base.t * Label.t * Region.t
  | TAlias  of Var.t
  | TTuple  of t * t
  | TRecord of (Var.t, t Option.t, Var.comparator_witness) Map.t
  | TArray  of t
  | TFun    of t * t

let maybe_to_string m f =
  match m with
  | None   -> "*"
  | Some v -> f v

let rec to_string t =
  match t with
  | TBase (tb, l, r) -> Printf.sprintf "%s<%s, %s>" (Base.to_string tb) (Label.to_string l) (Region.to_string r)
  | TAlias ta        -> Var.to_string ta
  | TTuple (t1, t2)  -> Printf.sprintf "(%s * %s)" (to_string t1) (to_string t2)
  | TRecord bs       ->
    Printf.sprintf "{ %s }"
      (String.concat
         (List.map
            (Map.to_alist bs)
            ~f:(fun (k, v) ->
                Printf.sprintf "%s : %s"
                  (Var.to_string k)
                  (maybe_to_string v to_string)))
         ~sep:"; ")
  | TArray t         -> Printf.sprintf "%s array" (to_string t)
  | TFun (t1, t2)    -> Printf.sprintf "%s -> %s" (to_string t1) (to_string t2)

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

let rec equal t1 t2 =
  match t1, t2 with
  | (TBase (tb1, l1, r1), TBase (tb2, l2, r2)) ->
    Base.equal tb1 tb2 &&
    Label.equiv l1 l2 &&
    Region.equiv r1 r2
  | (TAlias x, TAlias y) -> Var.equal x y
  | (TTuple (t11, t12), TTuple (t21, t22)) -> equal t11 t21 && equal t12 t22
  | (TRecord bs1, TRecord bs2) -> Map.equal (fun v1 v2 -> Option.equal equal v1 v2) bs1 bs2
  | (TArray t1, TArray t2) -> equal t1 t2
  | (TFun (t1_a, t1_b), TFun (t2_a, t2_b)) -> equal t1_a t2_a && equal t1_b t2_b
  | _ -> false

let rec resolve_alias t talias =
  Printf.printf "%s" (to_string t);
  match t with
  | TBase _   -> t
  | TAlias x -> resolve_alias (Map.find_exn talias x) talias
  | TTuple (t1, t2) -> TTuple (resolve_alias t1 talias, resolve_alias t2 talias)
  | TRecord bs -> TRecord (Map.map bs ~f:(fun v -> Option.map v ~f:(fun t' -> resolve_alias t' talias)))
  | TArray t' -> TArray (resolve_alias t' talias)
  | TFun (t1, t2) -> TFun (resolve_alias t1 talias, resolve_alias t2 talias)
