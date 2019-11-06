(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 * DongKwon Lee (dklee@ropas.snu.ac.kr)
 *)

open K
open Sm5
module Translator = struct

  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    | K.UNIT -> []
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.VAR id -> [Sm5.PUSH (Sm5.Id id); Sm5.LOAD]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.NOT e1 -> trans e1 @ [Sm5.NOT]
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.WRITE e -> trans e @ [Sm5.PUT]
    | K.LETV (x, e1, e2) ->
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.LETF (f, x, e1, e2) -> (*failwith "Unimplemented LETF" *)
      [Sm5.PUSH (Sm5.Fn (x, trans e1)); Sm5.BIND f] @
      trans e2 @ [Sm5.UNBIND]
    (*  *)
    | K.ASSIGN (x, e) -> trans e @ [Sm5.PUSH (Sm5.Id x); Sm5.STORE]
    | K.IF (e_cond, e_true, e_false) -> failwith "Unimplemented IF"
    | K.WHILE (e_cond, e_body) -> failwith "Unimplemented WHILE"
    | K.FOR (id, e1, e2, e_body) -> failwith "Unimplemented FOR"
    | K.SEQ (e1, e2) -> trans e1 @ trans e2
    | K.CALLV (f, arg_exp) ->
      [Sm5.PUSH (Sm5.Id f)] @ trans arg_exp @ [Sm5.MALLOC; Sm5.CALL]
    | K.CALLR (f, arg_var) -> failwith "Unimplemented CALLR"
    | _ -> failwith "Unimplemented"

end
