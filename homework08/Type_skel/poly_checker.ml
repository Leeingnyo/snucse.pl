(*
 * SNU 4190.310 Programming Languages 2017 Fall
 * Type Checker Skeleton
 *)

open M

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
  (*
  | TTypeList of typ list
  *)

let rec string_of_typ t = match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TPair (f, s) -> "pair(" ^ string_of_typ f ^ ", " ^ string_of_typ s ^ ")"
  | TLoc l -> "loc(" ^ string_of_typ l ^ ")"
  | TFun (a, b) -> "fun (" ^ string_of_typ a ^ ") -> (" ^ string_of_typ b ^ ")"
  | TVar id -> "var[" ^ id ^ "]"
  (*
  | TTypeList typ_list -> "list[" ^ String.concat ", " (List.map (fun x -> string_of_typ x) typ_list) ^ "]"
  *)

let rec translate_to_m_types t = match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TVar id -> raise (M.TypeError "no var to translate")
  | TPair (f, s) -> M.TyPair (translate_to_m_types f, translate_to_m_types s)
  | TLoc a -> M.TyLoc (translate_to_m_types a)
  | TFun (a, b) -> raise (M.TypeError "no type list to translate")
  (*
  | TTypeList typ_list -> raise (M.TypeError "no type list to translate")
  *)

type typ_scheme =
  | SimpleTyp of typ 
  | GenTyp of (var list * typ)

(* 내 감마 *)
type typ_env = (M.id * typ_scheme) list

(* Definitions related to free type variable *)

(* free type variable *)
let union_ftv ftv_1 ftv_2 = 
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2
  
let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v -> [v]

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas 

let ftv_of_env : typ_env -> var list = fun tyenv ->
  List.fold_left 
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv 

(* Generalize given type into a type scheme *)
let generalize : typ_env -> typ -> typ_scheme = fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp(ftv, t)

(* Definitions related to substitution *)

type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' = 
    match t' with
    | TVar x' -> if (x = x') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst alpha (TVar beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

let rec is_subtype var t = match t with
  | TInt | TBool | TString | TVar _ -> false
  | TLoc a -> if (a = var) then true else is_subtype var a
  | TFun (a, b) | TPair (a, b) -> if (a = var || b = var) then true else is_subtype var a || is_subtype var b
  (*
  | TTypeList typ_list -> List.fold_left (fun r -> fun e -> r || e) false (List.map (fun tt -> tt = t) typ_list)
  *)

let rec unify (tv1, tv2): subst =
  if tv1 = tv2 then empty_subst
  else
  match (tv1, tv2) with
  | (TVar a, b) | (b, TVar a) ->
    if (is_subtype (TVar a) b) then raise (M.TypeError "Fail to unify : circulation")
    else make_subst a b
  | (TPair (a1, a2), TPair (b1, b2)) (* 페어 같은 짝짝이면 짝짝끼리 해줍니다 *)
  | (TFun (a1, a2), TFun (b1, b2)) ->
    let s1 = unify (a1, b1) in
    let s2 = unify (s1 a2, s1 b2) in
    s2 @@ s1
  | (TLoc a, TLoc b) -> unify (a, b)
  (* not yet type *)
  | _ -> raise (M.TypeError "Fail to unify : otherwise")

let rec expensive : M.exp -> bool = fun e -> match e with
  | M.CONST _
  | M.VAR _
  | M.FN _
  | M.READ -> false
  | M.MALLOC _
  | M.APP _ -> true
  | M.IF (e1, e2, e3) -> expensive e1 || expensive e2 || expensive e3
  | M.LET (M.REC _, e)
  | M.WRITE e
  | M.BANG e
  | M.FST e
  | M.SND e -> expensive e
  | M.LET (M.VAL (_, e1), e2)
  | M.BOP (_, e1, e2)
  | M.ASSIGN (e1, e2)
  | M.SEQ (e1, e2)
  | M.PAIR (e1, e2) -> expensive e1 || expensive e2

let rec m (gamma, e, t): subst =
  match e with
  | M.CONST (M.S _) -> unify (t, TString)
  | M.CONST (M.N _) -> unify (t, TInt)
  | M.CONST (M.B _) -> unify (t, TBool)
  | M.VAR id ->
    (* 감마에서 찾은 다음 유니파이 *)
    unify (t,
      let typ_scheme = List.assoc id gamma in
      (match typ_scheme with
      | SimpleTyp t' -> t'
      | GenTyp (alphas, t') ->
        (List.fold_left (
          fun subst -> fun alpha ->
            let beta = new_var () in
            (* check *)
            make_subst alpha (TVar beta) @@ subst
          )
        empty_subst
        alphas
        ) t'
      )
    )
  | M.FN (x, e) ->
    let param_t = new_var () in
    let body_t = new_var () in
    let s1 = unify (t, TFun (TVar param_t, TVar body_t)) in
    let s2 = m ((x, SimpleTyp (s1 (TVar param_t))) :: (subst_env s1 gamma), e, s1 (TVar body_t)) in
    s2 @@ s1
  | M.APP (fn, arg) ->
    let a = new_var () in
    let s1 = m (gamma, fn, TFun (TVar a, t)) in
    let s2 = m (subst_env s1 gamma, arg, s1 (TVar a)) in
    s2 @@ s1
  | M.LET (dec, next) -> (match dec with
    | M.REC (f, x, e) ->
      let param_t = new_var () in
      let body_t = new_var () in
      let f_t = TFun (TVar param_t, TVar body_t) in
      let s1 = m ((f, SimpleTyp f_t) :: gamma, M.FN (x, e), f_t) in
      let gamma' = subst_env s1 gamma in
      let s2 = m ((f, generalize gamma' (s1 f_t)) :: gamma', next, s1 t) in
      s2 @@ s1
    | M.VAL (x, e) ->
      let a = new_var () in
      let s1 = m (gamma, e, TVar a) in
      let gamma' = subst_env s1 gamma in
      let x_t =
        if expensive e then SimpleTyp (s1 (TVar a))
        else generalize gamma' (s1 (TVar a))
      in
      let s2 = m ((x, x_t) :: gamma', next, s1 t) in
      s2 @@ s1
    )
  | M.IF (condition, true_body, false_body) ->
    let s1 = m (gamma, condition, TBool) in
    let gamma' = subst_env s1 gamma in
    let t' = s1 t in
    let s2 = m (gamma', true_body, t') in
    let gamma'' = subst_env s2 gamma' in
    let t'' = s2 t' in
    let s3 = m (gamma'', false_body, t'') in
    s3 @@ s2 @@ s1
  | M.BOP (op, e1, e2) ->
    (match op with
    | M.ADD | M.SUB ->
      let s1 = unify (t, TInt) in
      let s2 = m (subst_env s1 gamma, e1, TInt) in
      let s3 = m (subst_env s2 (subst_env s1 gamma), e2, TInt) in
      s3 @@ s2 @@ s1
    | M.AND | M.OR ->
      let s1 = unify (t, TBool) in
      let s2 = m (subst_env s1 gamma, e1, TBool) in
      let s3 = m (subst_env s2 (subst_env s1 gamma), e2, TBool) in
      s3 @@ s2 @@ s1
    | M.EQ ->
      let a = new_var () in
      let s1 = unify (t, TBool) in
      let s2 = m (subst_env s1 gamma, e1, TVar a) in
      let s3 = m (subst_env s2 (subst_env s1 gamma), e2, TVar a) in (* TODO comparable *)
      s3 @@ s2 @@ s1
    )
  | M.READ -> unify (t, TInt)
  | M.WRITE e ->
    m (gamma, e, t) (* TODO printable *)
  | M.MALLOC e ->
    let a = new_var () in
    let s1 = unify (t, TLoc (TVar a)) in
    let s2 = m (subst_env s1 gamma, e, TVar a) in
    s2 @@ s1
  | M.ASSIGN (dest, expr) ->
    let s1 = m (gamma, expr, t) in
    let s2 = m (subst_env s1 gamma, dest, TLoc (s1 t)) in
    s2 @@ s1
  | M.BANG loc ->
    let a = new_var () in
    let s1 = m (gamma, loc, TLoc (TVar a)) in
    let s2 = unify (t, s1 (TVar a)) in
    s2 @@ s1
  | M.SEQ (e1, e2) ->
    let a1 = new_var () in
    let a2 = new_var () in
    let s1 = unify (t, TVar a2) in
    let s2 = m (subst_env s1 gamma, e1, s1 (TVar a1)) in
    let s3 = m (subst_env s2 (subst_env s1 gamma), e2, s2 (s1 (TVar a2))) in
    s3 @@ s2 @@ s1
  | M.PAIR (fst, snd) ->
    let a1 = new_var () in
    let a2 = new_var () in
    let s1 = unify (t, TPair (TVar a1, TVar a2)) in
    let gamma' = subst_env s1 gamma in
    let s2 = m (gamma', fst, s1 (TVar a1)) in
    let gamma'' = subst_env s2 gamma' in
    let s3 = m (gamma'', snd, s2 (s1 (TVar a2))) in
    s3 @@ s2 @@ s1
  | M.FST pair ->
    let a = new_var () in
    m (gamma, pair, TPair (t, TVar a))
  | M.SND pair ->
    let a = new_var () in
    m (gamma, pair, TPair (TVar a, t))

let check : M.exp -> M.typ = fun e ->
  let tau = "x_tau" in
  let t = TVar tau in
  let substitution = m ([], e, t) in
  translate_to_m_types (substitution t)
