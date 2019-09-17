exception InvalidArgument
type ae = CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec diff (expression, variable) = match expression with
  | CONST i -> (CONST 0)
  | VAR str -> if str = variable then (CONST 1) else (CONST 0)
  | POWER (base, exponent) ->
    if base = variable then
      if exponent = 0 then (CONST 0)
      else if exponent = 1 then (CONST 1)
      else if exponent = 2 then (TIMES [(CONST exponent); (VAR base)])
      else (TIMES [(CONST exponent); (POWER (base, exponent - 1))])
    else (CONST 0)
  | TIMES ae_list -> if List.length ae_list = 0 then raise InvalidArgument
    else (SUM (List.map (fun e -> (TIMES ((diff (e, variable))::(List.filter (fun o -> o != e) ae_list)))) ae_list))
  | SUM ae_list -> if List.length ae_list = 0 then raise InvalidArgument
    else (SUM (List.map (fun e -> (diff (e, variable))) ae_list))
