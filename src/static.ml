open Error

type type_env = (Type.t Base.Option.t) Type.Env.t
type type_aliases = Type.t Type.Aliases.t

let rec static (scope : Scope.t) (constrs : Constrs.t) (tenv : type_env) (aliases : type_aliases) (e : Expr.t) : Type.t * type_env =
  match e.node with
  | Expr.ELit  l -> (TBase (Literal.to_type l.value, l.label, l.region), tenv)
  | Expr.EFlip f ->
    if Region.Expr.eq f.region Region.Expr.Bot then
      raise (TypeError (e.loc, "Region annotation cannot be bottom."))
    else
      (TBase (Type.Base.TBRBool, f.label, f.region), tenv)
  | Expr.ERnd  r ->
    if Region.Expr.eq r.region Region.Expr.Bot then
      raise (TypeError (e.loc, "Region annotation cannot be bottom."))
    else
      (TBase (Type.Base.TBRInt, r.label, r.region), tenv)
  | Expr.EVar x ->
    (try
       let x_t = Type.Env.find x.name tenv in
       match x_t with
       | None ->
         let msg = Printf.sprintf "Attempted to reference the variable %s, which has been consumed." (Var.to_string x.name) in
         raise (TypeError (e.loc, msg))
       | Some t ->
         match Type.accessible t with
         | Kind.Universal ->
           (t, tenv)
         | Kind.Affine ->
           (t, Type.Env.add x.name None tenv)
     with
     | Not_found ->
       let msg = Printf.sprintf "The variable %s is undefined." (Var.to_string x.name) in
       raise (TypeError (e.loc, msg)))
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
    let (t_bindings, tenv') = List.fold_left
        (fun (t_bindings_acc, env_acc) (field, binding) ->
           let (t, tenv_curr) = static scope constrs env_acc aliases binding in
           (Var.Map.add field t t_bindings_acc, tenv_curr))
        (Var.Map.empty, tenv)
        bindings.contents
    in
    (Type.TRecord t_bindings, tenv')
(* | Expr.ERecAcc comp ->
   let (t_rec, tenv') = static scope constrs tenv aliases comp.record in
   ...

   TODO(ins): Not sure how to type accessors, since they also need to be treated linearly.

   Proposal:

     < ENV = { } >
     type my_record = { foo : rbool ; bar : rbool } in
     < ENV = { } >
     let r = { foo = flip ; bar = flip } in
     < ENV = { r |-> { foo |-> rbool } } >
     r.foo
     < ENV = { r |-> { foo |-> * ; bar |-> rbool } } >

   Counterexample:

     < ENV = { } >
     type my_record = { foo : rbool ; bar : rbool } in
     < ENV = { } >
     let r1 = { foo = flip ; bar = flip } in
     < ENV = { r1 |-> { foo |-> rbool ; bar |-> rbool } } >
     let r2 = r1 in
     < ENV = { r1 |-> { foo |-> rbool ; bar |-> rbool } ; r2 |-> { foo |-> rbool ; bar |-> rbool } } >
     let x = r1.foo in
     < ENV = { r1 |-> { foo |-> * ; bar |-> rbool } ; r2 |-> { foo |-> rbool ; bar |-> rbool } } >
     let y = r2.foo in
     < ENV = { r1 |-> { foo |-> * ; bar |-> rbool } ; r2 |-> { foo |-> * ; bar |-> rbool } } >

   Notice that `x` and `y` now hold two copies of the affine value. This is bad. This is why records
   cannot be treated universally. Its kind must be the concat of all the kinds of its components.
*)
  | _ -> failwith "Unimplemented"
