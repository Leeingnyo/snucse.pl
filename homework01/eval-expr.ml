type expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
  | MULT of expr * expr
  | DIVIDE of expr * expr
  | MAX of expr list

let rec eval e = match e with
  | NUM i -> i
  | PLUS (a, b) -> (eval a) + (eval b)
  | MINUS (a, b) -> (eval a) - (eval b)
  | MULT (a, b) -> (eval a) * (eval b)
  | DIVIDE (a, b) -> (eval a) / (eval b)
  | MAX exprs ->
    if List.length exprs = 0 then 0
    else List.nth (List.sort (fun a -> fun b -> b - a)
        (List.map (fun x -> eval(x)) exprs)) 0 (* eval and sort and return first*)
