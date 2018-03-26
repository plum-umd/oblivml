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
