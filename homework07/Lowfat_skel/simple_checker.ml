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
  | TTypeList of typ list

let rec translate_to_m_types t = match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TVar id -> raise (M.TypeError "no var to translate")
  | TPair (f, s) -> M.TyPair (translate_to_m_types f, translate_to_m_types s)
  | TLoc a -> M.TyLoc (translate_to_m_types a)
  | TFun (a, b) -> M.TyArrow (translate_to_m_types a, translate_to_m_types b)
  | TTypeList typ_list -> raise (M.TypeError "no type list to translate")

let rec string_of_typ t = match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TPair (f, s) -> "pair(" ^ string_of_typ f ^ ", " ^ string_of_typ s ^ ")"
  | TLoc l -> "loc(" ^ string_of_typ l ^ ")"
  | TFun (a, b) -> "fun (" ^ string_of_typ a ^ ") -> (" ^ string_of_typ b ^ ")"
  | TVar id -> "var[" ^ id ^ "]"
  | TTypeList typ_list -> "list[" ^ String.concat ", " (List.map (fun x -> string_of_typ x) typ_list) ^ "]"

type formula =
  | Equal of typ * typ
  | And of formula * formula

let rec string_of_formula formula = match formula with
  | Equal (a, b) -> string_of_typ a ^ " = " ^ string_of_typ b
  | And (a, b) -> string_of_formula a ^ " ^ " ^ string_of_formula b

let rec v (gamma, e, t) = (match e with
  | M.CONST value -> (match value with
    | M.S _ -> Equal (TString, t)
    | M.N _ -> Equal (TInt, t)
    | M.B _ -> Equal (TBool, t)
    )
  | M.VAR id ->
      let (x, x_t) =
        (try (List.find (fun (x, x_t) -> x = id) gamma) with
        | Not_found -> raise (M.TypeError ("unboud " ^ id))
        )
      in
      Equal (t, x_t)
  | M.FN (x, e) ->
    let par_t = new_var () in
    let body_t = new_var () in
    And (Equal (t, TFun (TVar par_t, TVar body_t)), v ((x, TVar par_t) :: gamma, e, TVar body_t))
  | M.APP (fn, arg) ->
    let a = new_var () in
    And (v (gamma, fn, TFun (TVar a, t)), v (gamma, arg, TVar a))
  | M.LET (dec, next) -> (match dec with
    | M.REC (f, x, e) ->
      let par_t = new_var () in
      let body_t = new_var () in
      let f_t = TFun (TVar par_t, TVar body_t) in
      And (v ((f, f_t) :: gamma, M.FN (x, e), f_t), v ((f, f_t) :: gamma, next, t))
    | M.VAL (x, e) ->
      let a = new_var () in
      And (v ((x, TVar a) :: gamma, next, t), v (gamma, e, TVar a))
    )
  | M.IF (condition, true_body, false_body) ->
    And (v (gamma, condition, TBool), And (v (gamma, true_body, t), v (gamma, false_body, t)))
  | M.BOP (op, e1, e2) ->
    (match op with
    | M.ADD -> And (And (Equal (t, TInt), v (gamma, e1, TInt)), v (gamma, e2, TInt))
    | M.SUB -> And (And (Equal (t, TInt), v (gamma, e1, TInt)), v (gamma, e2, TInt))
    | M.EQ ->
      let a = new_var () in
      let b = new_var () in
      let l = new_var () in
      And (And (And (Equal (t, TBool), And (v (gamma, e1, TVar a), v (gamma, e2, TVar b))),
        Equal (TVar a, TVar b)),
        And (Equal (TTypeList [TLoc (TVar l); TInt; TString; TBool], TVar a), Equal (TTypeList [TLoc (TVar l); TInt; TString; TBool], TVar b)))
    | M.AND -> And (And (Equal (t, TBool), v (gamma, e1, TBool)), v (gamma, e2, TBool))
    | M.OR -> And (And (Equal (t, TBool), v (gamma, e1, TBool)), v (gamma, e2, TBool))
    )
  | M.READ -> Equal (t, TInt)
  | M.WRITE e -> And (v (gamma, e, t), Equal (t, TTypeList [TInt; TBool; TString]))
  | M.MALLOC e ->
    let a = new_var () in
    And (v (gamma, e, TVar a), Equal (t, TLoc (TVar a)))
  | M.ASSIGN (dest, expr) ->
    And (v (gamma, dest, TLoc t), v (gamma, expr, t))
  | M.BANG loc ->
    let a = new_var () in
    And (v (gamma, loc, TLoc (TVar a)), Equal (t, TVar a))
  | M.SEQ (e1, e2) ->
    let a1 = new_var () in
    let a2 = new_var () in
    And (And (v (gamma, e1, TVar a1), v (gamma, e2, TVar a2)), Equal (TVar a2, t))
  | M.PAIR (fst, snd) ->
    let a1 = new_var () in
    let a2 = new_var () in
    And (Equal (t, TPair (TVar a1, TVar a2)),
    And (v (gamma, fst, TVar a1), v (gamma, snd, TVar a2)))
  | M.FST pair ->
    let a = new_var () in
    v (gamma, pair, TPair (t, TVar a))
  | M.SND pair ->
    let a = new_var () in
    v (gamma, pair, TPair (TVar a, t))
  )

type equal_formula = EqualFormula of typ * typ

let string_of_equal_formula equal =
  match equal with
  | EqualFormula (a, b) -> string_of_typ a ^ " = " ^ string_of_typ b

let string_of_equal_formula_list equals =
  List.fold_left (fun str -> fun equal -> str ^ (if (str = "") then "" else " and ") ^ string_of_equal_formula equal) "" equals

let rec list_of_formula formula rr = match formula with
  | And (f1, f2) -> list_of_formula f1 [] @ list_of_formula f2 [] @ rr
  | Equal (a, b) -> EqualFormula (a, b) :: rr

let rec is_subtype var t = match t with
  | TInt | TBool | TString | TVar _ -> false
  | TLoc a -> if (a = var) then true else is_subtype var a
  | TFun (a, b) | TPair (a, b) -> if (a = var || b = var) then true else is_subtype var a || is_subtype var b
  | TTypeList typ_list -> List.fold_left (fun r -> fun e -> r || e) false (List.map (fun tt -> tt = t) typ_list)

let rec substitute_typ (t, v) typ =
  let change a = if (a = t) then v else substitute_typ (t, v) a in
  match typ with
  | TInt | TBool | TString -> typ
  | TVar _ -> if (typ = t) then v else typ
  | TLoc a -> TLoc (change a)
  | TFun (a, b) -> TFun (change a, change b)
  | TPair (a, b) -> TPair (change a, change b)
  | TTypeList typ_list -> TTypeList (List.map (fun x -> change x) typ_list)

let rec substitute (t, v) equals =
  match equals with
  | [] -> []
  | EqualFormula (a, b) :: left ->
    EqualFormula (substitute_typ (t, v) a, substitute_typ (t, v) b) :: substitute (t, v) left

(* u는 식들 모음, s는 계산된 타입 변수들 결과 모음 *)
let rec unify u s = match u with
  | [] -> s (* 끝났으면 끝~ *)
  | EqualFormula (a, b) :: left ->
    let is_loc = (fun x -> match x with | TLoc _ -> true | _ -> false) in
    let getLoc l = (match l with TLoc a -> a | _ -> raise (M.TypeError "this is not a loc")) in
    if a = b then unify left s (* a, b가 같으면 아무것도 안 하고 넘어갑니다 *)
    else (match (a, b) with
    | (TVar a, b) | (b, TVar a) -> (* 어느 한 쪽이 Variable 일 때 *)
      if (is_subtype (TVar a) b) then raise (M.TypeError "circular def") (* 서브 타입이면 에러 *)
      else unify (substitute (TVar a, b) left) ((TVar a, b) :: s) (* 이 변수는 저장하고 다른 식에서 이 변수를 쓰고 있으면 바꿔줍니다 *)
    | (TPair (a1, a2), TPair (b1, b2)) (* 페어 같은 짝짝이면 짝짝끼리 해줍니다 *)
    | (TFun (a1, a2), TFun (b1, b2)) -> unify (EqualFormula (a1, b1) :: EqualFormula (a2, b2) :: left) s
    | (TLoc a, TLoc b) -> unify (EqualFormula (a, b) :: left) s
    | (TTypeList typ_list, TInt) | (TInt, TTypeList typ_list) ->
      if List.mem TInt typ_list then unify left s else raise (M.TypeError "not matched")
    | (TTypeList typ_list, TBool) | (TBool, TTypeList typ_list) ->
      if List.mem TBool typ_list then unify left s else raise (M.TypeError "not matched")
    | (TTypeList typ_list, TString) | (TString, TTypeList typ_list) ->
      if List.mem TString typ_list then unify left s else raise (M.TypeError "not matched")
    | (TTypeList typ_list, TLoc a) | (TLoc a, TTypeList typ_list) ->
      (let l = (try List.find is_loc typ_list with Not_found -> raise (M.TypeError "not matched")) in
      unify (EqualFormula (TLoc a, l) :: left) s)
    | (TTypeList typ_list_a, TTypeList typ_list_b) ->
      if List.length typ_list_a != List.length typ_list_b then raise (M.TypeError "anything else fail")
      else
      (try
      let loca = List.find is_loc typ_list_a in
      let locb = List.find is_loc typ_list_b in
      unify (EqualFormula (getLoc loca, getLoc locb) :: left) s
      with Not_found -> unify left s
      )
    | _ -> (*let _ = print_endline (string_of_typ a ^ " ||||| " ^ string_of_typ b) in*) raise (M.TypeError "anything else fail")
    )

(* s에서 t를 뒤지는 함수입니다 *)
let rec calculate t s =
  (* vv에 다른 타입 변수가 있는 경우 대체해줘야 합니다 *)
  let rec trim vv = match vv with
    | TInt | TBool | TString -> vv
    | TVar _ -> calculate vv s
    | TLoc l -> TLoc (trim l)
    | TPair (a1, a2) -> TPair (trim a1, trim a2)
    | TFun (a1, a2) -> TFun (trim a1, trim a2)
    | TTypeList typ_list -> failwith "Unimplemented"
  in
  try trim ((fun (tt, vv) -> vv) (List.find (fun (tt, vv) -> t = tt) s)) with Not_found -> raise (M.TypeError ("I cant find " ^ (string_of_typ t) ^ " in S"))

(* TODO : Implement this function *)
let check : M.exp -> M.types = fun exp ->
  let tau = "@tau" in
  let formula = v ([], exp, TVar tau) in
  (* let _ = print_endline (string_of_formula formula) in *)
  let equals = list_of_formula formula [] in
  let s = unify (List.sort (fun a -> fun b -> match a with
    | EqualFormula (TTypeList _, _)
    | EqualFormula (_, TTypeList _) -> 1
    | _ -> -1
    ) equals) [] in
  translate_to_m_types (calculate (TVar tau) s)
