open Base
(* open Bit

module RExp = Rexp

type num = Int of int

let num_to_string (Int n) = string_of_int n

(* TODO: Make patterns recursive? E.g. to allow nested patterns. No idea how to do this. *)
type pattern =
  | XWild
  | XVar    of Var.t
  | XTuple  of Var.t * Var.t
  | XUnpack of Region.t list * Var.t
  | XRecord of (Field.t * Var.t) list
  | XFun    of Var.t * ((Var.t * Type.t) list) * Type.t option

let pattern_to_string p =
  match p with
    | XWild  -> "_"
    | XVar x -> Var.to_string x
    | XTuple (x, y) -> Printf.sprintf "(%s, %s)" (Var.to_string x) (Var.to_string y)
    | XUnpack (rs, x) ->
       Printf.sprintf
         "<%s . %s>"
         (String.concat
            ", "
            (List.map Region.to_string rs))
         (Var.to_string x)
    | XRecord fields ->
        Printf.sprintf "{ %s }" (String.concat
                                  ", "
                                  (List.map
                                    (fun (field, var) -> Printf.sprintf "%s = %s" (Field.to_string field) (Var.to_string var))
                                    fields))
    | XFun (name, args, ret) ->
        (Printf.sprintf "%s %s" (Var.to_string name) (String.concat
                                                      " "
                                                      (List.map
                                                        (fun (arg, typ) -> Printf.sprintf "(%s : %s)" (Var.to_string arg) (Type.to_string typ))
                                                        args)))
        ^ (match ret with
            | None -> ""
            | Some ret_t -> Printf.sprintf " : %s" (Type.to_string ret_t))
 *)

type section =
  { sect_start : Lexing.position
  ; sect_end   : Lexing.position
  }

type oblivml =
  { loc  : section option
  ; node : oblivml'
  }

and oblivml' =
  | EUnit
  | EBool of { value : Bool.t
             ; label : Label.t
             }
  | EVar  of { name  : Var.t
             }

(*
type expr =
  | EVar        of Var.t
  | EUnit
  | EBit        of Bit.t * Label.t
  | EInt        of num * Label.t
  | EPlus       of expr * expr
  | EMult       of expr * expr
  | EEq         of expr * expr
  | ENot        of expr
  | EAnd        of expr * expr
  | ETuple      of expr * expr
  | ERecord     of (Field.t * expr) list
  | EArrayInit  of expr * Var.t * expr
  | ERead       of expr * expr
  | EReadWrite  of expr * expr * expr
  | ELen        of expr
  | EToss
  | EUse        of Var.t
  | EReveal     of Var.t
  | EMux        of Kind.t * expr * expr * expr
  | EAlias      of Var.t * Type.t * expr
  | ELet        of pattern * expr * expr
  | ELetRec     of (pattern * expr) list * expr
  | EPack       of RExp.t list * Region.t list * expr * Type.t
  | EApp        of expr * expr
  | EIf         of expr * expr * expr

let rec expr_to_string e =
  match e with
    | EVar x -> Var.to_string x
    | EUnit -> "()"
    | EBit (b, l)     -> Printf.sprintf "%s %s" (Bit.to_string b) (Label.to_string l)
    | EInt (n, l)     -> Printf.sprintf "%s %s" (num_to_string n) (Label.to_string l)
    | EPlus (e1, e2)  -> Printf.sprintf "%s + %s" (expr_to_string e1) (expr_to_string e2)
    | EMult (e1, e2)  -> Printf.sprintf "%s * %s" (expr_to_string e1) (expr_to_string e2)
    | EEq   (e1, e2)  -> Printf.sprintf "%s = %s" (expr_to_string e1) (expr_to_string e2)
    | ENot e'         -> Printf.sprintf "!%s" (expr_to_string e')
    | EAnd  (e1, e2)  -> Printf.sprintf "%s && %s" (expr_to_string e1) (expr_to_string e2)
    | ETuple (e1, e2) -> Printf.sprintf "(%s, %s)" (expr_to_string e1) (expr_to_string e2)
    | ERecord fields  ->
        Printf.sprintf "{ %s }" (String.concat
                                  ", "
                                  (List.map
                                    (fun (field, e') -> Printf.sprintf "%s = %s" (Field.to_string field) (expr_to_string e'))
                                    fields))
    | EArrayInit (e1, v, e2) -> Printf.sprintf "array(%s)[%s -> %s]" (expr_to_string e1) (Var.to_string v) (expr_to_string e2)
    | ERead       (e1, e2)    -> Printf.sprintf "%s[%s]" (expr_to_string e1) (expr_to_string e2)
    | EReadWrite   (e1, e2, e3) -> Printf.sprintf "%s[%s] <- %s" (expr_to_string e1) (expr_to_string e2) (expr_to_string e3)
    | ELen  e'           -> Printf.sprintf "length(%s)" (expr_to_string e')
    | EToss                   -> Printf.sprintf "toss"
    | EUse       x            -> Printf.sprintf "use(%s)" (Var.to_string x)
    | EReveal    x            -> Printf.sprintf "reveal(%s)" (Var.to_string x)
    | EMux       (k, e1, e2, e3) -> Printf.sprintf "mux %s (%s, %s, %s)" (Kind.to_string k) (expr_to_string e1) (expr_to_string e2) (expr_to_string e3)
    | EAlias     (name, t, body) -> Printf.sprintf "type %s = %s in %s" (Var.to_string name) (Type.to_string t) (expr_to_string body)
    | ELet       (pat, e1, e2)   -> Printf.sprintf "let %s = %s in %s" (pattern_to_string pat) (expr_to_string e1) (expr_to_string e2)
    | ELetRec    (defs, rest)    -> Printf.sprintf "[ %s ] %s" (String.concat ", "
                                                              (List.map (fun (pat, body) -> Printf.sprintf "%s = %s" (pattern_to_string pat) (expr_to_string body)) defs))
                                                   (expr_to_string rest)
    | EPack (rexps, rs, e', t) -> Printf.sprintf
                                     "<%s as %s . %s : %s>"
                                     (String.concat
                                        ", "
                                        (List.map RExp.to_string rexps))
                                     (String.concat
                                        ", "
                                        (List.map Region.to_string rs))
                                     (expr_to_string e')
                                     (Type.to_string t)
    | EApp       (e1, e2)        -> Printf.sprintf "%s %s" (expr_to_string e1) (expr_to_string e2)
    | EIf        (e1, e2, e3)    -> Printf.sprintf "if %s then %s else %s" (expr_to_string e1) (expr_to_string e2) (expr_to_string e3)
 *)
