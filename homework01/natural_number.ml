type nat = ZERO | SUCC of nat
let rec natadd (a, b) = match a with | ZERO -> b | SUCC n -> (SUCC (add (n, b)))
let rec natmul (a, b) = match a with | ZERO -> ZERO | SUCC n -> (add (mul(n, b), b))
