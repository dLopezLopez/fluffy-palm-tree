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

(* Terms recognized by the program *)
type term =
    TmTrue of info					(* Boolean True *)
  | TmFalse of info					(* Boolean False *)
  | TmIf of info * term * term * term			(* If/then/else *)
  | TmVar of info * int * int				(* Variable *)
  | TmAbs of info * string * ty * term			(* Abstraction *)
  | TmApp of info * term * term				(* Application *)
  | TmRecord of info * (string * term) list		(* Record *)
  | TmProj of info * term * string			(* Projection *)
  | TmFloat of info * float				(* Float *)
  | TmTimesfloat of info * term * term			(* Product of floats *)
  | TmString of info * string				(* String *)
  | TmZero of info					(* Zero *)
  | TmSucc of info * term				(* Successor *)
  | TmPred of info * term				(* Predecessor *)
  | TmIsZero of info * term				(* IsZero *)
  | TmLet of info * string * ty *  term * term		(* Local variable *)
  | TmFix of info * term
  | TmLetRec of info * string * ty * term  * term

(* 2 types of binding. The standalone and the one associated with a term *)

type binding =
    NameBind
  | VarBind of ty
  | TmAbbBind of term * (ty option)
  | TyAbbBind of ty

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
val getTypeFromContext: info -> context -> int -> ty

(** Shifting and substitution **)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term
val typeShift: int -> ty -> ty


(** Printing **)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit
val printty : ty -> unit
val prbinding : context -> binding -> unit

(** Misc **)
val tmInfo: term -> info
