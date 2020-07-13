open Stdio
open Core

type t =
  { l_pos : Lexing.position
  ; r_pos : Lexing.position
  }

val empty : t

val to_string : t -> String.t

val pp : Format.formatter -> t -> unit
