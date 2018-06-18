open Core
open Stdio

exception TypeError of Section.t Option.t * String.t

type env_t = (Var.t, Type.t Option.t, Var.comparator_witness) Map.t

let rec static (tenv : env_t) (e : Expr.t) : Type.t * env_t =
  match e.node with
  | Expr.ELit  l ->
    (TBase (Literal.to_type l.value, l.label, l.region), tenv)
  | Expr.EFlip f ->
    if Region.equiv f.region Region.bottom then
      raise (TypeError (e.loc, "Region annotation cannot be bottom."))
    else
      (TBase (Type.Base.TBRBool, f.label, f.region), tenv)
  | Expr.ERnd  r ->
    if Region.equiv r.region Region.bottom then
      raise (TypeError (e.loc, "Region annotation cannot be bottom."))
    else
      (TBase (Type.Base.TBRInt, r.label, r.region), tenv)
  | Expr.EVar var ->
    let rec consume path record =
      match path with
      | [ ] -> failwith "Impossible: forbidden by lexer / parser."
      | [ x ] ->
        let x_t = Map.find_exn record x in
        (match x_t with
         | None ->
           let msg = Printf.sprintf "The variable %s on the path has already been consumed." (Var.to_string x) in
           raise (TypeError (e.loc, msg))
         | Some t ->
           (match Type.accessible t with
            | Kind.Universal -> (t, record)
            | Kind.Affine -> (t, Map.set record x None)))
      | x :: xs ->
        let x_t = Map.find_exn record x in
        (match x_t with
         | None ->
           let msg = Printf.sprintf "The variable %s on the path has already been consumed." (Var.to_string x) in
           raise (TypeError (e.loc, msg))
         | Some t ->
           (match t with
            | TRecord next ->
              consume xs next
            | _ ->
              let msg = Printf.sprintf "The variable %s on the path is not a record type, it has type %s." (Var.to_string x) (Type.to_string t) in
              raise (TypeError (e.loc, msg))))
    in
    consume var.path tenv
  | Expr.EBUnOp buo ->
    let ret = static tenv buo.arg in
    let (t_arg, tenv') = ret in
    (match t_arg with
     | Type.TBase (Type.Base.TBBool, _, _) -> ret
     | _ ->
       let msg = Printf.sprintf "Expected type `bool`, but got %s." (Type.to_string t_arg) in
       raise (TypeError (e.loc, msg)))
  | Expr.EBBinOp bbo ->
    let (t_lhs, tenv') = static tenv bbo.lhs in
    (match t_lhs with
     | Type.TBase (Type.Base.TBBool, l_lhs, r_lhs) ->
       let (t_rhs, tenv'') = static tenv' bbo.rhs in
       (match t_rhs with
        | Type.TBase (Type.Base.TBBool, l_rhs, r_rhs) ->
          (Type.TBase (Type.Base.TBBool, Label.join l_lhs l_rhs, Region.join r_lhs r_rhs), tenv'')
        | _ ->
          let msg = Printf.sprintf "Expected type `bool`, but got %s." (Type.to_string t_rhs) in
          raise (TypeError (bbo.rhs.loc, msg)))
     | _ ->
       let msg = Printf.sprintf "Expected type `bool`, but got %s." (Type.to_string t_lhs) in
       raise (TypeError (bbo.lhs.loc, msg)))
  | Expr.EAUnOp auo ->
    let ret = static tenv auo.arg in
    let (t_arg, tenv') = ret in
    (match t_arg with
     | Type.TBase (Type.Base.TBInt, _, _) -> ret
     | _ ->
       let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_arg) in
       raise (TypeError (e.loc, msg)))
  | Expr.EABinOp abo ->
    let (t_lhs, tenv') = static tenv abo.lhs in
    (match t_lhs with
     | Type.TBase (Type.Base.TBInt, l_lhs, r_lhs) ->
       let (t_rhs, tenv'') = static tenv' abo.rhs in
       (match t_rhs with
        | Type.TBase (Type.Base.TBInt, l_rhs, r_rhs) ->
          (Type.TBase (Type.Base.TBInt, Label.join l_lhs l_rhs, Region.join r_lhs r_rhs), tenv'')
        | _ ->
          let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_rhs) in
          raise (TypeError (abo.rhs.loc, msg)))
     | _ ->
       let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_lhs) in
       raise (TypeError (abo.lhs.loc, msg)))
  | Expr.EAUnRel aur ->
    let (t_arg, tenv') = static tenv aur.arg in
    (match t_arg with
     | Type.TBase (Type.Base.TBInt, l_arg, r_arg) ->
       (Type.TBase (Type.Base.TBBool, l_arg, r_arg), tenv')
     | _ ->
       let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_arg) in
       raise (TypeError (e.loc, msg)))
  | Expr.EABinRel abr ->
    let (t_lhs, tenv') = static tenv abr.lhs in
    (match t_lhs with
     | Type.TBase (Type.Base.TBInt, l_lhs, r_lhs) ->
       let (t_rhs, tenv'') = static tenv' abr.rhs in
       (match t_rhs with
        | Type.TBase (Type.Base.TBInt, l_rhs, r_rhs) ->
          (Type.TBase (Type.Base.TBBool, Label.join l_lhs l_rhs, Region.join r_lhs r_rhs), tenv'')
        | _ ->
          let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_rhs) in
          raise (TypeError (abr.rhs.loc, msg)))
     | _ ->
       let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_lhs) in
       raise (TypeError (abr.lhs.loc, msg)))
  | Expr.ETuple tup ->
    let (left, right) = tup.contents in
    let (t_left, tenv') = static tenv left in
    let (t_right, tenv'') = static tenv' right in
    (Type.TTuple (t_left, t_right), tenv'')
  | Expr.ERecord bindings ->
    let (t_bindings, tenv') =
      List.fold_left
        bindings.contents
        ~init:(Map.empty (module Var), tenv)
        ~f:(fun (t_bindings_acc, env_acc) (field, binding) ->
            let (t, tenv_curr) = static env_acc binding in
            (Map.set t_bindings_acc field (Some t), tenv_curr))
    in
    (Type.TRecord t_bindings, tenv')
  | Expr.EArrInit arr ->
    let (t_size, tenv') = static tenv arr.size in
    (match t_size with
     | Type.TBase (Type.Base.TBInt, l_size, r_size) when Label.equal l_size Label.bottom && Region.equiv r_size Region.bottom ->
       let (t_body, tenv'') = static tenv' arr.init in
       (match t_body with
        | Type.TFun (Type.TBase (Type.Base.TBInt, l_body, r_body), t_arr) when Label.equal l_body Label.bottom && Region.equiv r_body Region.bottom ->
          (TArray t_arr, tenv'')
        | _ ->
          let msg = Printf.sprintf "Expected type `int -> _`, but got %s." (Type.to_string t_body) in
          raise (TypeError (arr.init.loc, msg)))
     | _ ->
       let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_size) in
       raise (TypeError (arr.size.loc, msg)))
  | Expr.EArrRead read ->
    let (t_addr, tenv') = static tenv read.addr in
    (match t_addr with
     | Type.TArray t_ele ->
       let (t_idx, tenv'') = static tenv' read.idx in
       (match t_idx with
        | Type.TBase (Type.Base.TBInt, l_idx, r_idx) when Label.equal l_idx Label.bottom && Region.equiv r_idx Region.bottom ->
          (match Type.accessible t_ele with
           | Kind.Universal ->
             (t_ele, tenv'')
           | Kind.Affine ->
             let msg = Printf.sprintf "Attempting to read from %s (affine) array without a write." (Type.to_string t_ele) in
             raise (TypeError (e.loc, msg)))
        | _ ->
          let msg = Printf.sprintf "Array indices must be public. Yours has type %s." (Type.to_string t_idx) in
          raise (TypeError (read.idx.loc, msg)))
     | _ ->
       let msg = Printf.sprintf "Expected type `array _`, but got %s." (Type.to_string t_addr) in
       raise (TypeError (read.addr.loc, msg)))
  | Expr.EArrWrite write ->
    let (t_addr, tenv') = static tenv write.addr in
    (match t_addr with
     | Type.TArray t_ele ->
       let (t_idx, tenv'') = static tenv' write.idx in
       (match t_idx with
        | Type.TBase (Type.Base.TBInt, l_idx, r_idx) when Label.equal l_idx Label.bottom && Region.equiv r_idx Region.bottom ->
          let (t_value, tenv''') = static tenv'' write.value in
          if Type.equal t_value t_ele then
            (t_ele, tenv''')
          else
            let msg = Printf.sprintf "Attempting to write a value of type %s to an array of type %s." (Type.to_string t_value) (Type.to_string t_ele) in
            raise (TypeError (write.value.loc, msg))
        | _ ->
          let msg = Printf.sprintf "Array index must be a public int, got %s." (Type.to_string t_idx) in
          raise (TypeError (write.idx.loc, msg)))
     | _ ->
       let msg = Printf.sprintf "Expected type `array _`, but got %s." (Type.to_string t_addr) in
       raise (TypeError (write.addr.loc, msg)))
  | Expr.EArrLen len ->
    let (t_addr, tenv') = static tenv len.addr in
    (match t_addr with
     | Type.TArray _ ->
       (Type.TBase (Type.Base.TBInt, Label.bottom, Region.bottom), tenv')
     | _ ->
       let msg = Printf.sprintf "Expected type `array _`, but got %s." (Type.to_string t_addr) in
       raise (TypeError (len.addr.loc, msg)))
  | Expr.EUse use ->
    (try
       let x_t = Map.find_exn tenv use.arg in
       match x_t with
       | None ->
         let msg = Printf.sprintf "Attempted to reference the variable %s, which has been consumed." (Var.to_string use.arg) in
         raise (TypeError (e.loc, msg))
       | Some t ->
         (match t with
          | Type.TBase (Type.Base.TBRBool, l, r) ->
            (Type.TBase (Type.Base.TBBool, Label.top, r), tenv)
          | Type.TBase (Type.Base.TBRInt, l, r) ->
            (Type.TBase (Type.Base.TBInt, Label.top, r), tenv)
          | _ ->
            let msg = Printf.sprintf "The variable %s must be type `rbool` or `rint`, but is %s." (Var.to_string use.arg) (Type.to_string t) in
            raise (TypeError (e.loc, msg)))
     with
     | Not_found ->
       let msg = Printf.sprintf "The variable %s is undefined." (Var.to_string use.arg) in
       raise (TypeError (e.loc, msg)))
  | Expr.EReveal rev ->
    (try
       let x_t = Map.find_exn tenv rev.arg in
       match x_t with
       | None ->
         let msg = Printf.sprintf "Attempted to reference the variable %s, which has been consumed." (Var.to_string rev.arg) in
         raise (TypeError (e.loc, msg))
       | Some t ->
         (match t with
          | Type.TBase (Type.Base.TBRBool, l, r) ->
            (Type.TBase (Type.Base.TBBool, Label.bottom, Region.bottom), tenv)
          | Type.TBase (Type.Base.TBRInt, l, r) ->
            (Type.TBase (Type.Base.TBInt, Label.bottom, Region.bottom), tenv)
          | _ ->
            let msg = Printf.sprintf "The variable %s must be type `rbool` or `rint`, but is %s." (Var.to_string rev.arg) (Type.to_string t) in
            raise (TypeError (e.loc, msg)))
     with
     | Not_found ->
       let msg = Printf.sprintf "The variable %s is undefined." (Var.to_string rev.arg) in
       raise (TypeError (e.loc, msg)))
  | Expr.EMux mux ->
    let (t_guard, tenv') = static tenv mux.guard in
    (match t_guard with
     | Type.TBase (Type.Base.TBBool, l_guard, r_guard) ->
       let (t_lhs, tenv'') = static tenv' mux.lhs in
       let (t_rhs, tenv''') = static tenv'' mux.rhs in
       (* TODO(ins): FIXME *)
       failwith "TODO"
     | _ ->
       let msg = Printf.sprintf "The guard of this mux isn't a `bool` type, it has type %s." (Type.to_string t_guard) in
       raise (TypeError (mux.guard.loc, msg)))
  | _ -> failwith "!!"
