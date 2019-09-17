type formula = TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec expreval e = match e with
  | NUM i -> i
  | PLUS (a, b) -> expreval(a) + expreval(b)
  | MINUS (a, b) -> expreval(a) - expreval(b)

let rec eval f = match f with
  | TRUE -> true
  | FALSE -> false
  | NOT nf -> not (eval nf)
  | ANDALSO (a, b) -> eval(a) && eval(b)
  | ORELSE (a, b) -> eval(a) || eval(b)
  | IMPLY (a, b) -> if eval(a) then eval(b) else true
  | LESS (a, b) -> expreval(a) < expreval(b)
