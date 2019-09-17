module type Queue =
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
end

(*
L 넣을 거
R 뺄 거
*)
module IntListQ =
struct
  type element = int list
  type queue = element list * element list
  exception EMPTY_Q
  let emptyQ = ([], [])
  let enQ = fun ((l, r), e) -> List.append l (e::[]), r
  let deQ = fun (l, r) ->
    let get_last = fun l -> List.nth l ((List.length l) - 1) in
    let remove_last = fun l -> List.filter (fun e -> e != get_last l) l in (* <> 가 아님 *)
    let reverse_l = List.rev l in
    if (List.length l) = 0 && (List.length r) = 0 then raise EMPTY_Q
    else if (List.length r) = 0 then (get_last reverse_l, ([], remove_last reverse_l))
    else (get_last r, (l, remove_last r))
end
