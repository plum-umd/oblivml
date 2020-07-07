open Core

module type Reduction =
sig
  type value
  type env
  type store

  val value_to_string : value Value.t -> String.t

  val env_empty : env

  val store_empty : store

  val reduce : value Runtime.t -> env -> store -> (value Runtime.t * env * store) Or_error.t
end

module type S =
sig
  type value
  type c = value Runtime.t
  type e
  type s
  type k = (value, e) Cont.t


  type t = { c : c ; e : e ; s : s ; k : k }

  val eval : value Runtime.t -> t Or_error.t
end

module Make (P : Reduction) : S with type value = P.value
                                 and type e = P.env
                                 and type s = P.store =
struct
  type value = P.value
  type c = value Runtime.t
  type e = P.env
  type s = P.store
  type k = (value, e) Cont.t

  type t = { c : c ; e : e ; s : s ; k : k }

  let step m =
    if Runtime.is_redex m.c then
      let open Or_error.Let_syntax in
      let%bind (c', e', s') = P.reduce m.c m.e m.s in
      Or_error.return (Some { m with c = c' ; e = e' ; s = s' })
    else
    if Runtime.is_value m.c then
      if List.is_empty m.k then
        Or_error.return None
      else
        let (c', e', k') = Cont.pop m.c m.k in (* TODO(ins): This will need to take m.e as well, when we do substructural *)
        Or_error.return (Some { m with c = c' ; e = e' ; k = k' })
    else
      let (c', k') = Cont.push m.c m.e m.k in
      Or_error.return (Some { m with c = c' ; k = k' })

  let rec run' m =
    let open Or_error.Let_syntax in
    let%bind next = step m in
    match next with
    | None    -> Or_error.return m
    | Some m' -> run' m'

  let eval p = run' { c = p ; e = P.env_empty ; k = [] ; s = P.store_empty }
end
