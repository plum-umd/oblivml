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
     match t_lhs with
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
        raise (TypeError (bbo.lhs.loc, msg))
