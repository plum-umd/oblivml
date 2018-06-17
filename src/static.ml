open Core
open Stdio

exception TypeError of Section.t Option.t * String.t

type scope_t   = (Var.t, Var.comparator_witness) Set.t
type constrs_t = Unit.t
type env_t     = (Var.t, Type.t Option.t, Var.comparator_witness) Map.t
type aliases_t = (Var.t, Type.t, Var.comparator_witness) Map.t

let rec static (scope : scope_t) (constrs : constrs_t) (tenv : env_t) (aliases : aliases_t) (e : Expr.t) : Type.t * env_t =
  match e.node with
  | Expr.ELit  l ->
    (TBase (Literal.to_type l.value, l.label, l.region), tenv)
  | Expr.EFlip f ->
    if Region.Expr.equiv f.region Region.Expr.Bot then
      raise (TypeError (e.loc, "Region annotation cannot be bottom."))
    else
      (TBase (Type.Base.TBRBool, f.label, f.region), tenv)
  | Expr.ERnd  r ->
    if Region.Expr.equiv r.region Region.Expr.Bot then
      raise (TypeError (e.loc, "Region annotation cannot be bottom."))
    else
      (TBase (Type.Base.TBRInt, r.label, r.region), tenv)
  | Expr.EVar x ->
    (match x.path with
     | [] -> (* This is a normal variable *)
       (try
          let x_t = Map.find_exn tenv x.name in
          match x_t with
          | None ->
            let msg = Printf.sprintf "Attempted to reference the variable %s, which has been consumed." (Var.to_string x.name) in
            raise (TypeError (e.loc, msg))
          | Some t ->
            match Type.accessible t with
            | Kind.Universal ->
              (t, tenv)
            | Kind.Affine ->
              (t, Map.set tenv x.name None)
        with
        | Not_found ->
          let msg = Printf.sprintf "The variable %s is undefined." (Var.to_string x.name) in
          raise (TypeError (e.loc, msg)))
     | _ -> (* This is a record access *)
       (try
          let x_t = Map.find_exn tenv x.name in
          match x_t with
          | None ->
            let msg = Printf.sprintf "Attempted to reference the variable %s, which has been consumed." (Var.to_string x.name) in
            raise (TypeError (e.loc, msg))
          | Some t ->
            (match t with
             | TRecord record ->
               let rec consume_path path record =
                 match path with
                 | [] -> failwith "Impossible: initial path always has at least 1 element"
                 | [ y ] ->
                   let y_t = Map.find_exn record y in
                   (match y_t with
                    | None ->
                      let msg = Printf.sprintf "A variable on the path %s has already been consumed."
                          (String.concat ((Var.to_string x.name) :: (List.map path ~f:Var.to_string)) ~sep:".") in
                      raise (TypeError (e.loc, msg))
                    | Some t ->
                      (match Type.accessible t with
                       | Kind.Universal -> record
                       | Kind.Affine -> Map.set record y None))
                 | inner :: rest ->
                   let inner_t = Map.find_exn record inner in
                   (match inner_t with
                    | None ->
                      let msg = Printf.sprintf "A variable on the path %s has already been consumed."
                          (String.concat ((Var.to_string x.name) :: (List.map path ~f:Var.to_string)) ~sep:".") in
                      raise (TypeError (e.loc, msg))
                    | Some t ->
                      (match t with
                       | TRecord inner_record ->
                         consume_path rest inner_record
                       | _ ->
                         let msg = Printf.sprintf "Dot notation is only allowed for record types, %s has type %s." (Var.to_string inner) (Type.to_string t) in
                         raise (TypeError (e.loc, msg))))
               in
               let record' = consume_path x.path record in
               (TRecord record', tenv)
             | _ ->
               let msg = Printf.sprintf "Dot notation is only allowed for record types, %s has type %s." (Var.to_string x.name) (Type.to_string t) in
               raise (TypeError (e.loc, msg)))
        with
        | Not_found ->
          let msg = Printf.sprintf "The variable %s is undefined." (Var.to_string x.name) in
          raise (TypeError (e.loc, msg))))
  | Expr.EBUnOp buo ->
    let ret = static scope constrs tenv aliases buo.arg in
    let (t_arg, tenv') = ret in
    (match t_arg with
     | Type.TBase (Type.Base.TBBool, _, _) -> ret
     | _ ->
       let msg = Printf.sprintf "Expected type `bool`, but got %s." (Type.to_string t_arg) in
       raise (TypeError (e.loc, msg)))
  | Expr.EBBinOp bbo ->
    let (t_lhs, tenv') = static scope constrs tenv aliases bbo.lhs in
    (match t_lhs with
     | Type.TBase (Type.Base.TBBool, l_lhs, r_lhs) ->
       let (t_rhs, tenv'') = static scope constrs tenv' aliases bbo.rhs in
       (match t_rhs with
        | Type.TBase (Type.Base.TBBool, l_rhs, r_rhs) ->
          (Type.TBase (Type.Base.TBBool, Label.join l_lhs l_rhs, Region.Expr.Join (r_lhs, r_rhs)), tenv'')
        | _ ->
          let msg = Printf.sprintf "Expected type `bool`, but got %s." (Type.to_string t_rhs) in
          raise (TypeError (bbo.rhs.loc, msg)))
     | _ ->
       let msg = Printf.sprintf "Expected type `bool`, but got %s." (Type.to_string t_lhs) in
       raise (TypeError (bbo.lhs.loc, msg)))
  | Expr.EAUnOp auo ->
    let ret = static scope constrs tenv aliases auo.arg in
    let (t_arg, tenv') = ret in
    (match t_arg with
     | Type.TBase (Type.Base.TBInt, _, _) -> ret
     | _ ->
       let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_arg) in
       raise (TypeError (e.loc, msg)))
  | Expr.EABinOp abo ->
    let (t_lhs, tenv') = static scope constrs tenv aliases abo.lhs in
    (match t_lhs with
     | Type.TBase (Type.Base.TBInt, l_lhs, r_lhs) ->
       let (t_rhs, tenv'') = static scope constrs tenv' aliases abo.rhs in
       (match t_rhs with
        | Type.TBase (Type.Base.TBInt, l_rhs, r_rhs) ->
          (Type.TBase (Type.Base.TBInt, Label.join l_lhs l_rhs, Region.Expr.Join (r_lhs, r_rhs)), tenv'')
        | _ ->
          let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_rhs) in
          raise (TypeError (abo.rhs.loc, msg)))
     | _ ->
       let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_lhs) in
       raise (TypeError (abo.lhs.loc, msg)))
  | Expr.EAUnRel aur ->
    let (t_arg, tenv') = static scope constrs tenv aliases aur.arg in
    (match t_arg with
     | Type.TBase (Type.Base.TBInt, l_arg, r_arg) ->
       (Type.TBase (Type.Base.TBBool, l_arg, r_arg), tenv')
     | _ ->
       let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_arg) in
       raise (TypeError (e.loc, msg)))
  | Expr.EABinRel abr ->
    let (t_lhs, tenv') = static scope constrs tenv aliases abr.lhs in
    (match t_lhs with
     | Type.TBase (Type.Base.TBInt, l_lhs, r_lhs) ->
       let (t_rhs, tenv'') = static scope constrs tenv' aliases abr.rhs in
       (match t_rhs with
        | Type.TBase (Type.Base.TBInt, l_rhs, r_rhs) ->
          (Type.TBase (Type.Base.TBBool, Label.join l_lhs l_rhs, Region.Expr.Join (r_lhs, r_rhs)), tenv'')
        | _ ->
          let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_rhs) in
          raise (TypeError (abr.rhs.loc, msg)))
     | _ ->
       let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_lhs) in
       raise (TypeError (abr.lhs.loc, msg)))
  | Expr.ETuple tup ->
    let (left, right) = tup.contents in
    let (t_left, tenv') = static scope constrs tenv aliases left in
    let (t_right, tenv'') = static scope constrs tenv' aliases right in
    (Type.TTuple (t_left, t_right), tenv'')
  | Expr.ERecord bindings ->
    let (t_bindings, tenv') =
      List.fold_left
        bindings.contents
        ~init:(Map.empty (module Var), tenv)
        ~f:(fun (t_bindings_acc, env_acc) (field, binding) ->
           let (t, tenv_curr) = static scope constrs env_acc aliases binding in
           (Map.set t_bindings_acc field (Some t), tenv_curr))
    in
    (Type.TRecord t_bindings, tenv')
  | Expr.EArrInit arr ->
    let (t_size, tenv') = static scope constrs tenv aliases arr.size in
    (match t_size with
    | Type.TBase (Type.Base.TBInt, l_size, r_size) when Label.equal l_size Label.bottom && Region.Expr.equiv r_size Region.Expr.Bot ->
      let (t_body, tenv'') = static scope constrs tenv' aliases arr.init in
      (match t_body with
      | Type.TFun (Type.TBase (Type.Base.TBInt, l_body, r_body), t_arr) when Label.equal l_body Label.bottom && Region.Expr.equiv r_body Region.Expr.Bot ->
        (TArray t_arr, tenv'')
      | _ ->
          let msg = Printf.sprintf "Expected type `int -> _`, but got %s." (Type.to_string t_body) in
          raise (TypeError (arr.init.loc, msg)))
    | _ ->
      let msg = Printf.sprintf "Expected type `int`, but got %s." (Type.to_string t_size) in
      raise (TypeError (arr.size.loc, msg)))
  | Expr.EArrRead read ->
    let (t_addr, tenv') = static scope constrs tenv aliases read.addr in
    (match t_addr with
     | Type.TArray t_ele ->
       let (t_idx, tenv'') = static scope constrs tenv' aliases read.idx in
       (match t_idx with
        | Type.TBase (Type.Base.TBInt, l_idx, r_idx) when Label.equal l_idx Label.bottom && Region.Expr.equiv r_idx Region.Expr.Bot ->
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
  (* | Expr.EArrWrite write ->
    let (t_addr, tenv') = static scope constrs tenv aliases write.addr in
    (match t_addr with
     | Type.TArray t_ele ->
       let (t_idx, tenv'') = static scope constrs tenv' aliases write.idx in
       (match t_idx with
        | Type.TBase (Type.Base.TBInt, l_idx, r_idx) when Label.equal l_idx Label.bottom && Region.Expr.equiv r_idx Region.Expr.Bot ->
          let (t_value, tenv''') = static scope constrs tenv'' aliases write.value in
          if Type.equal t_value t_ele then
            (ele_t, tenv''')
          else
            let msg = Printf.sprintf "Attempting to write a value of type %s to an array of type %s." (Type.to_string t_value) (Type.to_string t_ele) in
            raise (TypeError (e.loc, msg))
        | _ ->
          let msg = Printf.sprintf "Array indices must be public. Yours has type %s." (Type.to_string t_idx) in
          raise (TypeError (read.idx.loc, msg)))
     | _ ->
       let msg = Printf.sprintf "Expected type `array _`, but got %s." (Type.to_string t_addr) in
       raise (TypeError (read.addr.loc, msg)))
  | Expr.EArrLen len ->
    let (t_addr, tenv') = static scope constrs tenv aliases len.addr in
    (match t_addr with
     | Type.TArray _ ->
       (Type.TBase (Type.Base.TBInt, Label.bottom, Region.Bot), tenv')
     | _ ->
       let msg = Printf.sprintf "Expected type `array _`, but got %s." (Type.to_string t_addr) in
       raise (TypeError (len.addr.loc, msg)))
  | Expr.EUse use ->
    (try
       let x_t = Type.Env.find use.name tenv in
       match x_t with
       | None ->
         let msg = Printf.sprintf "Attempted to reference the variable %s, which has been consumed." (Var.to_string use.name) in
         raise (TypeError (e.loc, msg))
       | Some t ->
         (match t with
          | Type.TBase (Type.Base.TBRBool, l, r) ->
            (Type.TBase (Type.Base.TBBool, Label.top, r), tenv)
          | Type.TBase (Type.Base.TBRInt, l, r) ->
            (Type.TBase (Type.Base.TBInt, Label.top, r), tenv)
          | _ ->

         match Type.accessible t with
         | Kind.Universal ->
           (t, tenv)
         | Kind.Affine ->
           (t, Type.Env.add x.name None tenv)
     with
     | Not_found ->
       let msg = Printf.sprintf "The variable %s is undefined." (Var.to_string x.name) in
       raise (TypeError (e.loc, msg)))) *)
  | _ -> failwith "!!"
