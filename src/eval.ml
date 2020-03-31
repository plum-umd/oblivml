open Core

module type Reduction =
sig
  type value
  type state

  val empty : state

  val reduce : value Runtime.t -> state -> (value Runtime.t * state) Or_error.t
end

module type S =
sig
  type value
  type c = value Runtime.t
  type k = value Cont.t
  type s

  type t = { c : c ; k : k ; s : s }

  val eval : value Runtime.t -> t Or_error.t
end

module Make (P : Reduction) : S with type value = P.value and type s = P.state =
struct
  type value = P.value
  type c = value Runtime.t
  type k = value Cont.t
  type s = P.state

  type t = { c : c ; k : k ; s : s }

  let step m =
    if Runtime.is_redex m.c then
      let open Or_error.Let_syntax in
      let%bind (c', s') = P.reduce m.c m.s in
      Or_error.return (Some { m with c = c' ; s = s' })
    else
    if Runtime.is_value m.c then
      if List.is_empty m.k then
        Or_error.return None
      else
        let (c', k') = Cont.pop m.c m.k in
        Or_error.return (Some { m with c = c' ; k = k' })
    else
      let (c', k') = Cont.push m.c m.k in
      Or_error.return (Some { m with c = c' ; k = k' })

  let rec run' m =
    let open Or_error.Let_syntax in
    let%bind next = step m in
    match next with
    | None    -> Or_error.return m
    | Some m' -> run' m'

  let eval e = run' { c = e ; k = [] ; s = P.empty }
end
