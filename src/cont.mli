open Core

(** Evaluation Contexts / Continuations *)
type 'v frame' =
  | KBOp         of { op : Boolean.Op.t ; evaluated : 'v Value.t List.t ; remaining : 'v Runtime.t List.t }
  | KAOp         of { op : Arith.Op.t ; evaluated : 'v Value.t List.t ; remaining : 'v Runtime.t List.t }
  | KARel        of { rel : Arith.Rel.t ; evaluated : 'v Value.t List.t ; remaining : 'v Runtime.t List.t }
  | KTupleL      of 'v Runtime.t
  | KTupleR      of 'v Value.t
  | KRecord      of { evaluated : (Var.t * 'v Value.t) List.t ; x : Var.t ; remaining : (Var.t * 'v Runtime.t) List.t }
  | KArrInitSz   of 'v Runtime.t
  | KArrInitInit of 'v Value.t
  | KArrReadLoc  of 'v Runtime.t
  | KArrReadIdx  of 'v Value.t
  | KArrWriteLoc of { idx : 'v Runtime.t ; value : 'v Runtime.t }
  | KArrWriteIdx of { loc : 'v Value.t ; value : 'v Runtime.t }
  | KArrWriteVal of { loc : 'v Value.t ; idx : 'v Value.t }
  | KArrLen
  | KMuxGuard    of { lhs : 'v Runtime.t ; rhs : 'v Runtime.t }
  | KMuxL        of { guard : 'v Value.t ; rhs : 'v Runtime.t }
  | KMuxR        of { guard : 'v Value.t ; lhs : 'v Value.t }
  | KAppLam      of 'v Runtime.t
  | KAppArg      of 'v Value.t
  | KLet         of { pat : Pattern.t ; body : 'v Runtime.t }
  | KIf          of { thenb : 'v Runtime.t ; elseb : 'v Runtime.t }
  | KPrint

type 'v frame = { source_location : Section.t
                ; datum : 'v frame' }

type 'v t = 'v frame List.t

val push : 'v Runtime.t -> 'v t -> 'v Runtime.t * 'v t

val pop : 'v Runtime.t -> 'v t -> 'v Runtime.t * 'v t
