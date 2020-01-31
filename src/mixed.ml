open Core
open Stdio

type t =
  | ELit      of { value  : Literal.t (** Literal *)
                 ; label  : Label.t
                 }

  | EVal      of value (** Values *)

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

and value =
  | VUnit   of Unit.t IDist.t
  | VBool   of { value : Bool.t IDist.t
               ; label : Label.t }
  | VInt    of { value : Int.t IDist.t
               ; label : Label.t }
  | VFlip   of Bool.t IDist.t
  | VRnd    of (Bool.t IDist.t) List.t
  | VLoc    of Loc.t
  | VAbs    of { param : Pattern.t
               ; body : t}
  | VRec    of { name : Var.t
               ; param : Pattern.t
               ; body : t }
  | VTuple  of (value, value) Tuple.T2.t
  | VRecord of (Var.t * value) list

type redex =
  | RLit of { value  : Literal.t (** Literal *)
            ; label  : Label.t
            }

  | RFlip (** Random Boolean *)

  | RRnd  (** Random Integer *)

  | RVar of { path : Var.t list  (** Variable *)
            }

  | RBUnOp of { op  : Boolean.Un.Op.t  (** Unary Boolean Operation *)
              ; arg : value
              }

  | RBBinOp   of { op  : Boolean.Bin.Op.t   (** Binary Boolean Operation *)
                 ; lhs : value
                 ; rhs : value
                 }

  | RAUnOp    of { op  : Arith.Un.Op.t  (** Unary Arithmetic Operation *)
                 ; arg : value
                 }

  | RABinOp   of { op  : Arith.Bin.Op.t   (** Binary Arithmetic Operation *)
                 ; lhs : value
                 ; rhs : value
                 }

  | RAUnRel   of { rel : Arith.Un.Rel.t   (** Unary Arithmetic Relation *)
                 ; arg : value
                 }

  | RABinRel  of { rel : Arith.Bin.Rel.t   (** Binary Arithmetic Relation *)
                 ; lhs : value
                 ; rhs : value
                 }

  | RTuple    of (value, value) Tuple.T2.t   (** Tuple *)

  | RRecord   of (Var.t * value) list   (** Record *)

  | RArrInit  of { size : value   (** Array Initialization *)
                 ; init : value
                 }

  | RArrRead  of { addr : value   (** Array Read *)
                 ; idx  : value
                 }

  | RArrWrite of { addr  : value   (** Array Write *)
                 ; idx   : value
                 ; value : value
                 }

  | RArrLen   of value   (** Array Length *)

  | RUse      of Var.t   (** Random -> Secret *)

  | RReveal   of Var.t   (** Random -> Public *)

  | RMux      of { guard : value   (** Mux *)
                 ; lhs   : value
                 ; rhs   : value
                 }

  | RAbs      of { param : Pattern.t   (** Abstraction *)
                 ; body  : t
                 }

  | RRec      of { name  : Var.t   (** Recursive Abstraction *)
                 ; param : Pattern.t
                 ; body  : t
                 }

  | RApp      of { lam : value   (** Application *)
                 ; arg : value
                 }

  | RLet      of { pat   : Pattern.t   (** Let-Binding *)
                 ; value : value
                 ; body  : t
                 }

  | RIf       of { guard : value   (** Conditional *)
                 ; thenb : t
                 ; elseb : t
                 }

type ectx =
  | KHole (** The Hole [] *)

  | KBUnOp of { op   : Boolean.Un.Op.t (** Unary Boolean Operation *)
              ; cont : ectx
              }

  | KBBinOpL of { op   : Boolean.Bin.Op.t (** Binary Boolean Operation (Left Evaluation) *)
                ; cont : ectx
                ; rhs  : t
                }

  | KBBinOpR of { op   : Boolean.Bin.Op.t (** Binary Boolean Operation (Right Evaluation) *)
                ; lhs  : value
                ; cont : ectx
                }

  | KAUnOp of { op   : Arith.Un.Op.t (** Unary Arithmetic Operation *)
              ; cont : ectx
              }

  | KABinOpL of { op   : Arith.Bin.Op.t (** Binary Arithmetic Operation (Left Evaluation) *)
                ; cont : ectx
                ; rhs  : t
                }

  | KABinOpR of { op   : Arith.Bin.Op.t   (** Binary Arithmetic Operation (Right Evaluation) *)
                ; lhs  : value
                ; cont : ectx
                }

  | KAUnRel of { rel  : Arith.Un.Rel.t   (** Unary Arithmetic Relation *)
               ; cont : ectx
               }

  | KABinRelL of { rel  : Arith.Bin.Rel.t   (** Binary Arithmetic Relation (Left Evaluation) *)
                 ; cont : ectx
                 ; rhs  : t
                 }

  | KABinRelR of { rel  : Arith.Bin.Rel.t   (** Binary Arithmetic Relation (Right Evaluation) *)
                 ; lhs  : value
                 ; cont : ectx
                 }

  | KTupleL of (ectx, t) Tuple.T2.t   (** Tuple (Left Evaluation) *)

  | KTupleR of (value, ectx) Tuple.T2.t   (** Tuple (Right Evaluation) *)

  | KRecord of value List.t * ectx * t List.t (** Record (Left to Right Evaluation) *)

  | KArrInitSz of { cont : ectx
                  ; init : t
                  }

  | KArrInitV of { size : value
                 ; cont : ectx
                 }

  | KArrReadAddr of { cont : ectx
                    ; idx : t
                    }

  | KArrReadIdx of { addr : value
                   ; cont : ectx
                   }

  | KArrWriteAddr of { cont : ectx
                     ; idx : t
                     ; value : t }

  | KArrWriteIdx of { addr : value
                    ; cont : ectx
                    ; value : t
                    }

  | KArrWriteVal of { addr : value
                    ; idx : value
                    ; cont : ectx
                    }

  | KArrLen of { cont : ectx }

  | KMuxGuard of { cont : ectx
                 ; lhs : t
                 ; rhs : t
                 }

  | KMuxL of { guard : value
             ; cont : ectx
             ; rhs : t
             }

  | KMuxR of { guard : value
             ; lhs : value
             ; cont : ectx
             }

  | KAbs of { param : Pattern.t
            ; cont : ectx
            }

  | KRec of { name : Var.t
            ; param : Pattern.t
            ; cont : ectx
            }

  | KAppF of { cont : ectx
             ; arg : t
             }

  | KAppA of { lam  : value
             ; cont : ectx
             }

  | KLetV of { pat : Pattern.t
             ; cont : ectx
             ; body : t
             }

  | KLetB of { pat : Pattern.t
             ; value : value
             ; cont : ectx
             }

  | KIfG of { cont : ectx
            ; thenb : t
            ; elseb : t
            }
