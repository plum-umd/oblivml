open Stdio
open Core

type t =
  { l_pos : Lexing.position
  ; r_pos : Lexing.position
  }

val to_string : t -> String.t

val pp : Format.formatter -> t -> unit
