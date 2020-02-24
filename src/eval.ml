open Core

module Make (P : Primitive.S) : Eval_intf.S with type value = P.value and type s = P.t =
struct
  type value = P.value
  type c = value Runtime.t
  type k = value Cont.t
  type s = P.t

  type t = { c : c ; k : k ; s : s }

  let step m =
    if Runtime.is_redex m.c then
      let (c', s') = P.reduce m.c m.s in
      Some { m with c = c' ; s = s' }
    else
    if Runtime.is_value m.c then
      if List.is_empty m.k then
        None
      else
        let (c', k') = Cont.pop m.c m.k in
        Some { m with c = c' ; k = k' }
    else
      let (c', k') = Cont.push m.c m.k in
      Some { m with c = c' ; k = k' }

  let rec run' m =
    match step m with
    | None    -> m
    | Some m' -> run' m'

  let eval e = run' { c = e ; k = [] ; s = P.empty }
end
