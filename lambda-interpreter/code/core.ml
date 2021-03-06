(*
   Práctica realizada por Mateo Gende Lozano y Diego Suarez García,
   alumnos de Diseño de Lenguajes de Programación en el curso 2017/18,
   tomando como base las implementaciones que Benjamin C. Pierce cita
   en su libro Types and Programming Languages.
*)

(******************************************************************************
 *
 * Module Core
 *
 * Core typechecking and evaluation functions
 *
 *****************************************************************************)

open Format
open Syntax
open Support.Error
open Support.Pervasive

(** ------------------------   EVALUATION  ------------------------ **)

exception NoRuleApplies

let debug = ref false

(* isnumericval returns true if term t is a number or false if not *)
let rec isnumericval ctx t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval ctx t1
  | _ -> false

(* isval returns true if the term t is a value or false if not *)
let rec isval ctx t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | TmFloat _  -> true
  | TmString _  -> true
  | t when isnumericval ctx t  -> true
  | TmAbs(_,_,_,_) -> true
  | TmRecord(_,fields) -> List.for_all (fun (l,ti) -> isval ctx ti) fields
  | _ -> false

(* eval1 evaluates one step forward the term t *)
let rec eval1 ctx t = match t with
    TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(fi, t1', t2, t3)
  | TmVar(fi,n,_) ->
      (match getbinding fi ctx n with
          TmAbbBind(t,_) -> t
        | _ -> raise NoRuleApplies)
  | TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmRecord(fi,fields) ->
      let rec evalafield l = match l with
        [] -> raise NoRuleApplies
      | (l,vi)::rest when isval ctx vi ->
          let rest' = evalafield rest in
          (l,vi)::rest'
      | (l,ti)::rest ->
          let ti' = eval1 ctx ti in
          (l, ti')::rest
      in let fields' = evalafield fields in
      TmRecord(fi, fields')
  | TmProj(fi, (TmRecord(_, fields) as v1), l) when isval ctx v1 ->
      (try List.assoc l fields
       with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1' = eval1 ctx t1 in
      TmProj(fi, t1', l)
  | TmTimesfloat(fi,TmFloat(_,f1),TmFloat(_,f2)) ->
      TmFloat(fi, f1 *. f2)
  | TmTimesfloat(fi,(TmFloat(_,f1) as t1),t2) ->
      let t2' = eval1 ctx t2 in
      TmTimesfloat(fi,t1,t2')
  | TmTimesfloat(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmTimesfloat(fi,t1',t2)
  | TmSucc(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmIsZero(fi, t1')
  | TmLet(fi,x,tty,v1,t2) when isval ctx v1 ->
      termSubstTop v1 t2
  | TmLet(fi,x,tty,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmLet(fi, x,tty ,t1', t2)
  | TmFix(fi,v1) as t when isval ctx v1 ->
     (match v1 with
        TmAbs(_,_,_,t12) -> termSubstTop t t12
      | _ -> raise NoRuleApplies)
  | TmFix(fi,t1) ->
      let t1' = eval1 ctx t1
      in TmFix(fi,t1')
  | TmLetRec(fi,x, tty,v1, t2) when isval ctx v1 ->
      termSubstTop v1 t2
  | TmLetRec(fi,x,tty,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmLetRec(fi,x,tty, t1', t2)
  | _ ->
      raise NoRuleApplies

(* eval evaluates the term t until it cannot be evaluated anymore. If debug mode is activated,
   it also prints how the terms are being evaluated *)
let rec eval ctx t =
  if !debug then begin
    if not (isval ctx t) then begin pr "("; printtm ctx t; pr ") -> "
    end else ();
      try let t' = eval1 ctx t;
      in eval ctx t'
    with NoRuleApplies -> t
  end else
    try let t' = eval1 ctx t;
      in eval ctx t'
    with NoRuleApplies -> t

(* evalbinding evaluates the term contained by the binding until it cannot be evaluated anymore *)
let evalbinding ctx b = match b with
    TmAbbBind(t,tyT) ->
      let t1 = eval ctx t in
      TmAbbBind(t1,tyT)
  | bind -> bind



let gettyabb ctx i =
  match getbinding dummyinfo ctx i with
   TyAbbBind(tyT) -> tyT
  | _ -> raise NoRuleApplies

let istyabb ctx i =
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> true
  | _ -> false

let rec computety ctx tyT = match tyT with
    TyVar(i,_) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise NoRuleApplies

let rec simplifyty ctx tyT =
  try
    let tyT' = computety ctx tyT in
    simplifyty ctx tyT'
  with NoRuleApplies -> tyT

let rec typeof ctx t =
  match t with
    TmZero(_) ->
      TyNat
    | TmSucc(fi,t) ->
      (if (=) (typeof ctx t) TyNat then
        TyNat
      else error fi "parameter type mismatch")
    | TmPred(fi,t) ->
      (if (=) (typeof ctx t) TyNat then
        TyNat
      else error fi "parameter type mismatch")
    | TmIsZero(fi,t) ->
      (if (=) (typeof ctx t) TyNat then
        TyBool
      else error fi "parameter type mismatch")
    | TmTrue(_) ->
      TyBool
    | TmFalse(_) ->
      TyBool
    | TmIf(fi,t1,t2,t3) ->
      if (=) (typeof ctx t1) TyBool then
        let tyT2 = typeof ctx t2 in
        if (=) tyT2 (typeof ctx t3) then tyT2
        else error fi "arms of conditional have different type"
      else error fi "guard of conditional is not a bolean"
    | TmVar(fi,i,_) -> getTypeFromContext fi ctx i
    | TmAbs(fi,x,tyT1,t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1,tyT2)
    | TmLet(fi,x,tty,t1,t2) ->
     let tyT1 = typeof ctx t1 in
        if (=) tyT1 tty then
           let ctx' = addbinding ctx x (VarBind(tyT1)) in
           typeShift (-1) (typeof ctx' t2)
        else error fi "Error type"
    | TmFix(fi, t1) ->
    let tyT1 = typeof ctx t1 in
    (match simplifyty ctx tyT1 with
         TyArr(tyT11,tyT12) ->
           if (=) tyT12 tyT11 then tyT12
           else error fi "result of body not compatible with domain"
       | _ -> error fi "arrow type expected")
    | TmLetRec(fi,x,tty,t1,t2) ->
      let tyT1 = typeof ctx t1 in
        if (=) tyT1 tty then
          (let ctx' = addbinding ctx x (VarBind(tyT1)) in
          typeShift (-1) (typeof ctx' t2))
        else error fi "Error type"
    | TmApp (fi,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
        TyArr(tyT11,tyT12) ->
          if (=) tyT2 tyT11 then tyT12
          else error fi "parameter type mismatch"
          | _ -> error fi "arrow type expected")
;;
