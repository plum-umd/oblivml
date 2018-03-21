(* Impossible Errors *)

exception Impossible

(* Syntax Errors *)
open Lexing

exception SyntaxError of position * string

let string_of_pos { pos_fname = f;
                    pos_lnum  = l;
                    pos_bol   = b;
                    pos_cnum  = c } =
  Printf.sprintf "File \"%s\", line %d, column %d\n" f l (c - b)

(* Errors we know how to handle *)
type error =
  | ErrUnimpl
  | ErrSyntax

let error_to_code (e : error) : int =
  match e with
  | ErrUnimpl -> 3
  | ErrSyntax -> 4

let error_to_string (e : error) : string =
  match e with
  | ErrUnimpl -> "Unimplemented"
  | ErrSyntax -> "Syntax Error"

let handle (e : error) (msg : string) =
  Printf.eprintf "%s: %s\n" (error_to_string e) msg;
  exit (error_to_code e)
