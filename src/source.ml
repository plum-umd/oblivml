open Core
open Stdio

type t = {
  source_location  : Section.t;
  datum : t'
}

and t' =
  | ELit of {
      datum : Literal.t;
      label : Label.t
    } (** Literal *)

  | EFlip of Region.t (** Random Boolean *)

  | ERnd of Region.t (** Random Integer *)

  | EVar of Var.t List.t (** Variable *)

  | EBOp of {
      op : Boolean.Op.t;
      args : t List.t
    } (** Boolean Operation *)

  | EAOp of {
      op : Arith.Op.t;
      args : t List.t
    } (** Arithmetic Operation *)

  | EARel of {
      rel : Arith.Rel.t;
      args : t List.t
    } (** Arithmetic Relation *)

  | ETuple of (t, t) Tuple.T2.t (** Tuple *)

  | ERecord of (Var.t * t) List.t (** Record *)

  | EArrInit of {
      size : t;
      init : t
    } (** Array Initialization *)

  | EArrRead of {
      loc : t;
      idx : t
    } (** Array Read *)


  | EArrWrite of {
      loc : t;
      idx : t;
      value : t
    } (** Array Write *)

  | EArrLen of t (** Array Length *)

  | ECast of {
      var : Var.t;
      label : Label.t
    } (** Cast *)

  | EMux of {
      guard : t;
      lhs : t;
      rhs : t
    } (** Mux *)

  | EAbs of {
      pat : Pattern.t;
      body : t
    } (** Abstraction *)

  | ERec of {
      name : Var.t;
      pat : Pattern.t;
      body : t;
      t_ret : Type.t
    } (** Recursive Abstraction *)

  | EApp of {
      lam : t;
      arg : t
    } (** Application *)

  | ELet of {
      pat : Pattern.t;
      value : t;
      body : t
    } (** Let-Binding *)

  | EType of {
      name : Var.t;
      typ : Type.t;
      body : t
    } (** Type Alias *)

  | EIf of {
      guard : t;
      thenb : t;
      elseb : t
    } (** Conditional *)

  | EPrint of t (** Print *)
