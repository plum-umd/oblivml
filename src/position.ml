open Core
open Stdio

open Lexing

type t = position

let to_string { pos_fname = f;
                pos_lnum  = l;
                pos_bol   = b;
                pos_cnum  = c } =
  Printf.sprintf "File \"%s\", line %d, column %d\n" f l (c - b)
