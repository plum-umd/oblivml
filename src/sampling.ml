open Core
open Stdio

type region = (Int.t, Int.comparator_witness) Set.t

type t =
  { loc : Section.t Option.t
  ; node : t'
  }

and t' =
  | ELit      of { value  : Literal.t (** Literal *)
                 ; label  : Label.t
                 }

  | EFlip (** Random Boolean *)

  | ERnd  (** Random Integer *)

  | EVar      of { path : Var.t list  (** Variable *)
                 }

  | EBUnOp    of { op  : Boolean.Un.Op.t  (** Unary Boolean Operation *)
                 ; arg : t
                 }

  | EBBinOp   of { op  : Boolean.Bin.Op.t   (** Binary Boolean Operation *)
                 ; lhs : t
                 ; rhs : t
                 }

  | EAUnOp    of { op  : Arith.Un.Op.t  (** Unary Arithmetic Operation *)
                 ; arg : t
                 }

  | EABinOp   of { op  : Arith.Bin.Op.t   (** Binary Arithmetic Operation *)
                 ; lhs : t
                 ; rhs : t
                 }

  | EAUnRel   of { rel : Arith.Un.Rel.t   (** Unary Arithmetic Relation *)
                 ; arg : t
                 }

  | EABinRel  of { rel : Arith.Bin.Rel.t   (** Binary Arithmetic Relation *)
                 ; lhs : t
                 ; rhs : t
                 }

  | ETuple    of (t, t) Tuple.T2.t   (** Tuple *)

  | ERecord   of (Var.t * t) list   (** Record *)

  | EArrInit  of { size : t   (** Array Initialization *)
                 ; init : t
                 }

  | EArrRead  of { addr : t   (** Array Read *)
                 ; idx  : t
                 }


  | EArrWrite of { addr  : t   (** Array Write *)
                 ; idx   : t
                 ; value : t
                 }

  | EArrLen   of t   (** Array Length *)

  | EUse      of Var.t   (** Random -> Secret *)

  | EReveal   of Var.t   (** Random -> Public *)

  | EMux      of { guard : t   (** Mux *)
                 ; lhs   : t
                 ; rhs   : t
                 }

  | EAbs      of { param : Pattern.t   (** Abstraction *)
                 ; body  : t
                 }

  | ERec      of { name  : Var.t   (** Recursive Abstraction *)
                 ; param : Pattern.t
                 ; body  : t
                 }

  | EApp      of { lam : t   (** Application *)
                 ; arg : t
                 }

  | ELet      of { pat   : Pattern.t   (** Let-Binding *)
                 ; value : t
                 ; body  : t
                 }

  | EIf       of { guard : t   (** Conditional *)
                 ; thenb : t
                 ; elseb : t
                 }
  | EPrint    of t

and value =
  | VUnit
  | VBool   of { value : Bool.t
               ; label : Label.t
               ; region : region }
  | VInt    of { value : Int.t
               ; label : Label.t
               ; region : region }
  | VFlip   of { value : Bool.t
               ; region : region }
  | VRnd    of { value : Int.t
               ; region : region }
  | VLoc    of Loc.t
  | VAbs    of { env : (Var.t, value Ref.t, Var.comparator_witness) Map.t
               ; param : Pattern.t
               ; body : t}
  | VTuple  of (value, value) Tuple.T2.t
  | VRecord of (Var.t * value) list

(** Erase a source term into a runtime term *)
let rec of_source (e : Expr.t) : t =
  let node =
    match e.node with
    | ELit l -> ELit { value = l.value ; label = l.label }
    | EFlip _ -> EFlip
    | ERnd _ -> ERnd
    | EVar p -> EVar { path = p.path }
    | EBUnOp b -> EBUnOp { op = b.op ; arg = of_source b.arg }
    | EBBinOp b -> EBBinOp { op = b.op ; lhs = of_source b.lhs ; rhs = of_source b.rhs }
    | EAUnOp a -> EAUnOp { op = a.op ; arg = of_source a.arg }
    | EABinOp a -> EABinOp { op = a.op ; lhs = of_source a.lhs ; rhs = of_source a.rhs }
    | EAUnRel a -> EAUnRel { rel = a.rel ; arg = of_source a.arg }
    | EABinRel a -> EABinRel { rel = a.rel ; lhs = of_source a.lhs ; rhs = of_source a.rhs }
    | ETuple (l, r) -> ETuple (of_source l, of_source r)
    | ERecord fields -> ERecord (List.map ~f:(fun (x, e) -> (x, of_source e)) fields)
    | EArrInit sp -> EArrInit { size = of_source sp.size ; init = of_source sp.init }
    | EArrRead sp -> EArrRead { addr = of_source sp.addr ; idx = of_source sp.idx }
    | EArrWrite sp -> EArrWrite { addr = of_source sp.addr ; idx = of_source sp.idx ; value = of_source sp.value }
    | EArrLen len -> EArrLen (of_source len)
    | EUse x -> EUse x
    | EReveal x -> EReveal x
    | ETrust x -> EVar { path = [x] }
    | EProve x -> EVar { path = [x] }
    | EMux m -> EMux { guard = of_source m.guard ; lhs = of_source m.lhs ; rhs = of_source m.rhs }
    | EAbs f -> EAbs { param = f.param ; body = of_source f.body }
    | ERec f -> ERec { name = f.name ; param = f.param ; body = of_source f.body }
    | EApp ap -> EApp { lam = of_source ap.lam ; arg = of_source ap.arg }
    | ELet l -> ELet { pat = l.pat ; value = of_source l.value ; body = of_source l.body }
    | EType t -> (of_source t.body).node
    | EIf ite -> EIf { guard = of_source ite.guard ; thenb = of_source ite.thenb ; elseb = of_source ite.elseb }
    | EPrint e -> EPrint (of_source e)
  in
  { loc = e.loc ; node = node }

let denote_buo op arg =
  match arg with
  | VBool b ->
    let v' =
      (match op with
       | Boolean.Un.Op.Not -> not b.value)
    in
    VBool { b with value = v' }
  | _ -> failwith "Boolean Unary Op type error"

let denote_bbo op arg1 arg2 =
  match arg1, arg2 with
  | VBool b1, VBool b2 ->
    let v' =
      (match op with
       | Boolean.Bin.Op.And -> b1.value && b2.value)
    in
    VBool { value = v' ; label = Label.join b1.label b2.label ; region = Set.union b1.region b2.region }
  | _ -> failwith "Boolean Binary Op type error"

let denote_auo op arg = failwith "No such operators"

let denote_abo op arg1 arg2 =
  match arg1, arg2 with
  | VInt n1, VInt n2 ->
    let op =
      (match op with
       | Arith.Bin.Op.Add -> ( + )
       | Arith.Bin.Op.Subtract -> ( - )
       | Arith.Bin.Op.Mult -> ( * )
       | Arith.Bin.Op.Div -> ( / )
       | Arith.Bin.Op.Mod -> ( % )
       | Arith.Bin.Op.And -> ( land ))
    in
    let v' = op n1.value n2.value in
    VInt { value = v' ; label = Label.join n1.label n2.label ; region = Set.union n1.region n2.region }
  | _ -> failwith "Boolean Binary Op type error"

let denote_aur rel arg = failwith "No such relations"

let denote_abr rel arg1 arg2 =
  match arg1, arg2 with
  | VInt n1, VInt n2 ->
    let rel =
      (match rel with
       | Arith.Bin.Rel.Equal -> Int.equal)
    in
    let v' = rel n1.value n2.value in
    VBool { value = v' ; label = Label.join n1.label n2.label ; region = Set.union n1.region n2.region }
  | _ -> failwith "Arithmetic Binary Relation type error"

let value_to_string = function
  | VUnit -> "()"
  | VBool b -> Printf.sprintf "%s" (Bool.to_string b.value)
  | VInt n -> Printf.sprintf "%s" (Int.to_string n.value)
  | _ -> failwith "TODO"
