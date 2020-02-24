module type S =
sig
  type value
  type c = value Runtime.t
  type k = value Cont.t
  type s

  type t = { c : c ; k : k ; s : s }

  val eval : value Runtime.t -> t
end
