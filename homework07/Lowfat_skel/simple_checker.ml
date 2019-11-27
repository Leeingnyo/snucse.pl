(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type var = string

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  (* Modify, or add more if needed *)

type formula =
  | Equal of typ * typ
  | And of formula * formula

let rec is_valid f = match f with
  | Equal (a, b) -> a = b
  | And (f1, f2) -> is_valid f1 && is_valid f2

let rec v (gamma, e, t) = (match e with
  | M.CONST value -> (match value with
    | M.S _ -> Equal (TString, t)
    | M.N _ -> Equal (TInt, t)
    | M.B _ -> Equal (TBool, t)
    )
  | M.VAR id -> Equal (TVar id, t) (* id 인 걸 gamma에서 찾기 *)
  | M.FN (par, body) -> failwith "Unimplemented FN"
  | M.APP (fn, arg) -> failwith "Unimplemented APP"
  | M.LET (dec, next) -> (match dec with
    | M.REC (f, par, body) -> failwith "Unimplemented LET REC"
    | M.VAL (x, e) -> failwith "Unimplemented LET VAL"
    )
  | M.IF (condition, true_body, false_body) ->
    And (v (gamma, condition, TBool), And (v (gamma, true_body, t), v (gamma, false_body, t)))
  | M.BOP (op, e1, e2) ->
    (match op with
    | M.ADD -> And (And (Equal (t, TInt), v (gamma, e1, TInt)), v (gamma, e2, TInt))
    | M.SUB -> And (And (Equal (t, TInt), v (gamma, e1, TInt)), v (gamma, e2, TInt))
    | M.EQ ->
      let a = new_var () in
      And (Equal (t, TBool), And (v (gamma, e1, TVar a), v (gamma, e2, TVar a)))
    | M.AND -> And (And (Equal (t, TBool), v (gamma, e1, TBool)), v (gamma, e2, TBool))
    | M.OR -> And (And (Equal (t, TBool), v (gamma, e1, TBool)), v (gamma, e2, TBool))
    )
  | M.READ -> Equal (t, TInt)
  | M.WRITE e -> v (gamma, e, t)
  | M.MALLOC e -> failwith "Unimplemented MALLOC"
  | M.ASSIGN (dest, expr) -> failwith "Unimplemented MALLOC"
  | M.BANG loc -> failwith "Unimplemented MALLOC"
  | M.SEQ (e1, e2) -> failwith "Unimplemented MALLOC"
  | M.PAIR (fst, snd) -> failwith "Unimplemented MALLOC"
  | M.FST pair -> failwith "Unimplemented MALLOC"
  | M.SND pair -> failwith "Unimplemented MALLOC"
  )

let rec string_of_typ t = match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TPair (f, s) -> "pair(" ^ string_of_typ f ^ ", " ^ string_of_typ s ^ ")"
  | TLoc l -> "loc(" ^ string_of_typ l ^ ")"
  | TFun (a, b) -> "fun (" ^ string_of_typ a ^ ") -> (" ^ string_of_typ b ^ ")"
  | TVar id -> "var[" ^ id ^ "]"
let rec string_of_formula formula = match formula with
  | Equal (a, b) -> string_of_typ a ^ " = " ^ string_of_typ b
  | And (a, b) -> string_of_formula a ^ " ^ " ^ string_of_formula b

(* TODO : Implement this function *)
let check : M.exp -> M.types = fun exp ->
  let tau = "tau" in
  let formula = v ([], exp, TVar tau) in
  let _ = print_endline (string_of_formula formula) in
  M.TyInt
  (* raise (M.TypeError "Type Checker Unimplemented") *)
