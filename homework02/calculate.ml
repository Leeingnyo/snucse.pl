exception FreeVariable
type exp = X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp

type expression =
  | FORMULA of (float -> float)
  | VALUE of (float -> float)

(* (expression, float) -> float *)
let eval (e, x) = match e with
  | FORMULA f -> f x
  | VALUE f -> f x

(* exp -> expression *)
let rec build e = match e with
  | X -> FORMULA (fun x -> x)
  | INT i -> VALUE (fun x -> float_of_int i)
  | REAL f -> VALUE (fun x -> f)
  | ADD (a, b) -> (match (build a, build b) with
    | (VALUE _a, VALUE _b) -> VALUE (fun x -> eval (build a, x) +. eval (build b, x))
    | (_, _) -> FORMULA (fun x -> eval (build a, x) +. eval (build b, x))
    )
  | SUB (a, b) -> (match (build a, build b) with
    | (VALUE _a, VALUE _b) -> VALUE (fun x -> eval (build a, x) -. eval (build b, x))
    | (_, _) -> FORMULA (fun x -> eval (build a, x) -. eval (build b, x))
    )
  | MUL (a, b) -> (match (build a, build b) with
    | (VALUE _a, VALUE _b) -> VALUE (fun x -> eval (build a, x) *. eval (build b, x))
    | (_, _) -> FORMULA (fun x -> eval (build a, x) *. eval (build b, x))
    )
  | DIV (a, b) -> (match (build a, build b) with
    | (VALUE _a, VALUE _b) -> VALUE (fun x -> eval (build a, x) /. eval (build b, x))
    | (_, _) -> FORMULA (fun x -> eval (build a, x) /. eval (build b, x))
    )
  | SIGMA (a, b, f) -> (VALUE (fun x ->
    let ea = int_of_float (eval (build a, x)) in
    let eb = int_of_float (eval (build b, x)) in
    if ea > eb then 0. else (eval ((build f), (eval (build a, x))) +.
        (eval (build (SIGMA (INT (ea + 1), INT eb, f)), x)))
    ))
  | INTEGRAL (a, b, f) -> VALUE (fun x ->
    let ea = (eval (build a, x)) in
    let eb = (eval (build b, x)) in
    if ea > eb then -1. *. eval (build (INTEGRAL (b, a, f)), x)
    else if ea = eb then 0.
    else (eval (build f, ea) *. 0.1 +. eval (build (INTEGRAL (REAL (0.1 +. ea), b, f)), x))
    )

(* exp -> float *)
let calculate e = match (build e) with
  | FORMULA f -> raise FreeVariable
  | VALUE f -> f 0.
