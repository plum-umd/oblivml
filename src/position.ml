open Core
open Stdio

open Lexing

type t = position

let to_string { pos_fname = f
              ; pos_lnum  = l
              ; pos_bol   = b
              ; pos_cnum  = c } =
  Printf.sprintf "File \"%s\", line %d, column %d" f l (c - b)

let pp f p = Format.pp_print_text f (to_string p)
