(*
   Práctica realizada por Mateo Gende Lozano y Diego Suarez García,
   alumnos de Diseño de Lenguajes de Programación en el curso 2017/18,
   tomando como base las implementaciones que Benjamin C. Pierce cita
   en su libro Types and Programming Languages.
*)

(** Module Syntax: syntax trees and associated support functions **)

open Support.Pervasive
open Support.Error

(** Data type definitions **)
type ty =
    TyArr of ty * ty
  | TyBool
  | TyNat
  | TyVar of int * int

type term =
    TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmFloat of info * float
  | TmTimesfloat of info * term * term
  | TmString of info * string
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmLet of info * string * ty * term * term
  | TmLetRec of info * string * ty * term * term


  type binding =
      NameBind
    | VarBind of ty
    | TmAbbBind of term * (ty option)

type command =
  | Eval of info * term
  | Bind of info * string * binding

(** Context **)
type context = (string * binding) list
val emptycontext : context
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addname: context -> string -> context
val index2name : info -> context -> int -> string
val getbinding : info -> context -> int -> binding
val name2index : info -> context -> string -> int
val isnamebound : context -> string -> bool


(** Shifting and substitution **)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term

(** Printing **)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit
val printty : ty -> unit
val prbinding : context -> binding -> unit

(** Misc **)
val tmInfo: term -> info

val typeof: context -> term -> ty

val fix : ty -> info -> string -> context -> term
