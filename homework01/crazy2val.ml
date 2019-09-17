type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val crazy = match crazy with NIL -> 0
  | ZERO zero -> 0 + 2 * crazy2val(zero)
  | ONE one -> 1 + 2 * crazy2val(one)
  | MONE mone -> -1 + 2 * crazy2val(mone)
