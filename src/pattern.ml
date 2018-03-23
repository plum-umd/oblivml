type t =
  | XWild
  | XVar of Var.t
  | XTuple of t * t
  | XRecord of (Var.t * t) list
  | XAscr of t * Type.t

let rec to_string p =
  match p with
  | XWild  -> "_"
  | XVar v -> Var.to_string v
  | XTuple (p1, p2) -> Printf.sprintf "(%s, %s)" (to_string p1) (to_string p2)
  | XRecord fs -> Printf.sprintf "{ %s }"
                                 (String.concat ", "
                                                (List.map
                                                   (fun (k, v) ->
                                                     Printf.sprintf "%s = %s" (Var.to_string k) (to_string v))
                                                   fs))
  | XAscr (p, t) -> Printf.sprintf "(%s : %s)" (to_string p) (Type.to_string t)

let pp f p = Format.pp_print_text f (to_string p)
