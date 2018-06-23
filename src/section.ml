open Core
open Stdio

type t =
  { l_pos : Position.t
  ; r_pos : Position.t
  }

let to_string s =
  Printf.sprintf
    "%s - %s"
    (Position.to_string s.l_pos)
    (Position.to_string s.r_pos)

let pp f s = Format.pp_print_text f (to_string s)
