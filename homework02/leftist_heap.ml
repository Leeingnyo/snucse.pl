exception EmptyHeap
type heap = EMPTY | NODE of rank * value * heap * heap
  and rank = int
  and value = int

let rank h = match h with
  | EMPTY -> -1
  | NODE (r, _, _, _) -> r
let shake (x, lh, rh) =
  if (rank lh) >= (rank rh)
  then NODE ((rank rh) + 1, x, lh, rh)
  else NODE ((rank lh) + 1, x, rh, lh)

(* target
 * merge : heap * heap -> heap
 *)
let rec merge (lh, rh) = match (lh, rh) with
  | (EMPTY, EMPTY) -> EMPTY
  | (EMPTY, NODE (_, _, _, _)) -> rh
  | (NODE (_, _, _, _), EMPTY) -> lh
  | (NODE (lh_rank, lh_value, lh_left_child, lh_right_child), NODE (rh_rank, rh_value, rh_left_child, rh_right_child)) ->
    if lh_value < rh_value then shake (lh_value, lh_left_child, merge(lh_right_child, rh))
    else shake (rh_value, rh_left_child, merge(lh, rh_right_child))

let insert (x, h) = merge (h, NODE (0, x, EMPTY, EMPTY))
let findMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE (_, x, _, _) -> x
let deleteMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE (_, x, lh, rh) -> merge (lh, rh)
