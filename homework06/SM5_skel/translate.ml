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
    | K.UNIT -> [Sm5.PUSH (Sm5.Val Sm5.Unit)]
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
    | K.WRITE e -> trans e @ [Sm5.MALLOC; Sm5.BIND "-write"; Sm5.PUSH (Sm5.Id "-write"); Sm5.STORE; Sm5.PUSH (Sm5.Id "-write"); Sm5.LOAD; Sm5.PUSH (Sm5.Id "-write"); Sm5.LOAD; Sm5.PUT; Sm5.UNBIND; Sm5.POP]
    | K.LETV (x, e1, e2) ->
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.LETF (f, x, e1, e2) ->
      [Sm5.PUSH (Sm5.Fn (x, [Sm5.BIND f] @ trans e1 @ [Sm5.UNBIND; Sm5.POP])); Sm5.BIND f] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.ASSIGN (x, e) -> trans e @ [Sm5.MALLOC; Sm5.BIND "-assign"; Sm5.PUSH (Sm5.Id "-assign"); Sm5.STORE; Sm5.PUSH (Sm5.Id "-assign"); Sm5.LOAD; Sm5.PUSH (Sm5.Id "-assign"); Sm5.LOAD; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.UNBIND; Sm5.POP]
    | K.IF (e_cond, e_true, e_false) ->
      trans e_cond @ [Sm5.JTR (trans e_true, trans e_false)]
    | K.WHILE (e_cond, e_body) -> trans (K.LETF ("-while", "-cond", K.IF (K.VAR "-cond", K.SEQ(e_body, K.CALLV ("-while", e_cond)), K.UNIT), (K.CALLV ("-while", e_cond))))
      (* f(cond) { if (cond) body; f(cond) } f(cond)  *)
    | K.FOR (id, e1, e2, e_body) -> trans (
        K.LETV ("-i", e1,
        K.LETV ("-n", e2,
        K.LETF ("-for", "-i",
          K.IF (K.LESS (K.VAR "-n", K.VAR "-i"), K.UNIT,
            K.SEQ (
              K.ASSIGN (id, K.VAR "-i"),
            K.SEQ (
              e_body,
              K.CALLV ("-for", K.ADD (K.VAR "-i", K.NUM 1))
            ))
          ),
        K.CALLV ("-for", K.VAR "-i")
        )))
      )
      (*
        [i] := e1;
        [n] := e2;
        f(ii) { if ([n] < [i]) unit else { e_body; [i] = ii + 1; f(ii + 1) } }
        f([i]);
      *)
    | K.SEQ (e1, e2) -> trans e1 @ [Sm5.POP] @ trans e2
    | K.CALLV (f, arg_exp) ->
      [Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f)] @
      trans arg_exp @ [Sm5.MALLOC; Sm5.CALL]
    | K.CALLR (f, arg_var) ->
      [Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id arg_var); Sm5.LOAD; Sm5.PUSH (Sm5.Id arg_var); Sm5.CALL]
    | K.TRY (e, h) -> [Sm5.TRYSTART (trans e, trans h)]
    | K.RAISE -> [Sm5.RAISE]
    | _ -> failwith "Unimplemented"

end
