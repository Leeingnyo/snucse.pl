type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val crazy = match crazy with NIL -> 0
  | ZERO next -> 0 + 2 * crazy2val(next)
  | ONE next -> 1 + 2 * crazy2val(next)
  | MONE next -> -1 + 2 * crazy2val(next)

let rec crazy2add (a, b) = match a with
  | NIL -> b
  | ZERO a_next -> (match b with
    | NIL -> a
    | ZERO b_next -> ZERO (crazy2add(a_next, b_next))
    | ONE b_next -> ONE (crazy2add(a_next, b_next))
    | MONE b_next -> MONE (crazy2add(a_next, b_next)))
  | ONE a_next -> (match b with
    | NIL -> a
    | ZERO b_next -> ONE (crazy2add(a_next, b_next))
    | ONE b_next -> ZERO (crazy2add(ONE NIL, crazy2add(a_next, b_next)))
    | MONE b_next -> ZERO (crazy2add(a_next, b_next)))
  | MONE a_next -> (match b with
    | NIL -> a
    | ZERO b_next -> MONE (crazy2add(a_next, b_next))
    | ONE b_next -> ZERO (crazy2add(a_next, b_next))
    | MONE b_next -> ZERO (crazy2add(MONE NIL, crazy2add(a_next, b_next))))
