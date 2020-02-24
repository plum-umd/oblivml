module type S =
sig
  type value
  type t

  val empty : t

  val reduce : value Runtime.t -> t -> (value Runtime.t * t)
end
