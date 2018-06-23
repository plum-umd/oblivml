type t =
  { l_pos : Lexing.position
  ; r_pos : Lexing.position
  }

val pp : Format.formatter -> t -> unit
