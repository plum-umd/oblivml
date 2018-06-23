open Core
open Stdio

exception PatternError
exception BindingError of Var.t * String.t
exception AscriptionError of Type.t * Type.t

type t =
  | XWild
  | XVar of Var.t
  | XTuple of t * t
  | XRecord of (Var.t * t) list
  | XAscr of t * Type.t

let merge_disjoint m1 m2 =
  Map.merge
      m1
      m2
      ~f:(fun ~key:x v ->
          match v with
          | `Left v1     -> (Some v1)
          | `Right v2    -> (Some v2)
          | `Both (_, _) -> raise (BindingError (x, "duplicate binding")))

let rec get_binders p t =
  match p, t with
  | (XWild, _)                              -> Map.empty (module Var)
  | (XVar x, _)                             -> Map.singleton (module Var) x (Some t)
  | (XTuple (p1, p2), Type.TTuple (t1, t2)) ->
    let bs1 = get_binders p1 t1 in
    let bs2 = get_binders p2 t2 in
    merge_disjoint bs1 bs2
  | (XRecord ps, Type.TRecord bindings)     ->
    List.fold
      ps
      ~init:(Map.empty (module Var))
      ~f:(fun m (f, pi) ->
          match Map.find bindings f with
          | None   -> raise (BindingError (f, "field not present in record."))
          | Some v ->
            (match v with
             | None    -> raise (BindingError (f, "field has already been consumed."))
             | Some ti -> merge_disjoint m (get_binders pi ti)))
  | (XAscr (p', t_assert), t') ->
    if Type.equal t_assert t' then
      get_binders p' t'
    else
      raise (AscriptionError (t_assert, t'))
  | _ ->
    raise PatternError

let rec to_string p =
  match p with
  | XWild           -> "_"
  | XVar v          -> Var.to_string v
  | XTuple (p1, p2) -> Printf.sprintf "(%s, %s)" (to_string p1) (to_string p2)
  | XRecord fs      -> Printf.sprintf "{ %s }"
                         (String.concat
                            ~sep:", "
                            (List.map
                               fs
                               ~f:(fun (k, v) ->
                                   Printf.sprintf "%s = %s" (Var.to_string k) (to_string v))))
  | XAscr (p, t)    -> Printf.sprintf "(%s : %s)" (to_string p) (Type.to_string t)

let pp f p = Format.pp_print_text f (to_string p)
