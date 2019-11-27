(*
 * SNU 4190.310 Programming Languages 
 * Homework "Rozetta" Skeleton
 *)

let trans_v : Sm5.value -> Sonata.value = function
  | Sm5.Z z  -> Sonata.Z z
  | Sm5.B b  -> Sonata.B b
  | Sm5.L _ -> raise (Sonata.Error "Invalid input program : pushing location")
  | Sm5.Unit -> Sonata.Unit
  | Sm5.R _ -> raise (Sonata.Error "Invalid input program : pushing record")

(* TODO : complete this function *)
let rec trans_obj : Sm5.obj -> Sonata.obj = function
  | Sm5.Val v -> Sonata.Val (trans_v v)
  | Sm5.Id id -> Sonata.Id id
  | Sm5.Fn (arg, command) -> Sonata.Fn (arg,
    (* left :: stack, { ... env }  *)
    (* stack, { ... env, left: left }  *)
    trans' ([Sm5.BIND "@my-left-work"] @ command @ [Sm5.PUSH (Sm5.Id "@my-left-work"); Sm5.PUSH (Sm5.Val Sm5.Unit); Sm5.MALLOC; Sm5.CALL]))

(* TODO : complete this function *)
and trans' : Sm5.command -> Sonata.command = function
  | Sm5.PUSH obj :: cmds -> Sonata.PUSH (trans_obj obj) :: (trans' cmds)
  | Sm5.POP :: cmds -> Sonata.POP :: (trans' cmds)
  | Sm5.STORE :: cmds -> Sonata.STORE :: (trans' cmds)
  | Sm5.LOAD :: cmds -> Sonata.LOAD :: (trans' cmds)
  | Sm5.JTR (c1, c2) :: cmds -> [Sonata.JTR (trans' (c1 @ cmds), trans' (c2 @ cmds))]
  | Sm5.MALLOC :: cmds -> Sonata.MALLOC :: (trans' cmds)
  | Sm5.BOX z :: cmds -> Sonata.BOX z :: (trans' cmds)
  | Sm5.UNBOX id :: cmds -> Sonata.UNBOX id :: (trans' cmds)
  | Sm5.BIND id :: cmds -> Sonata.BIND id :: (trans' cmds)
  | Sm5.UNBIND :: cmds -> Sonata.UNBIND :: (trans' cmds)
  | Sm5.GET ::cmds -> Sonata.GET :: (trans' cmds)
  | Sm5.PUT ::cmds -> Sonata.PUT :: (trans' cmds)
  | Sm5.CALL :: cmds ->
    (match cmds with
    | [] -> [Sonata.CALL] (* 함수로 안 만들어도 됨 걍 콜 해 *)
    | _ -> [
      Sonata.PUSH (Sonata.Fn ("!fu@k", [Sonata.UNBIND; Sonata.POP] @ trans' cmds)); (* 내 뒷 일 *)
      Sonata.BIND "@@@fuxk@@@";
      (* l :: v :: f :: stack, { ... env } *)
      Sonata.BIND "!loc-loc";
      (* v :: f :: stack, { ... env, !loc: l } *)
      Sonata.MALLOC; Sonata.BIND "!val-val"; Sonata.PUSH (Sonata.Id "!val-val"); Sonata.STORE;
      (* f :: stack, { ... env, !loc: l, !val: new-l }, mem { new-l -> v *)
      Sonata.BIND "!func-func";
      (* stack, { ... env, !loc: l, !val: new-l, !func: f }  *)
      (* 3개 저장 *)
      Sonata.PUSH (Sonata.Id "@@@fuxk@@@");
      (* left :: stack, { ... env, !loc: l, !val: new-l, !func: f }  *)
      Sonata.PUSH (Sonata.Id "!func-func"); Sonata.UNBIND; Sonata.POP;
      (* f :: left :: stack, { ... env, !loc: l, !val: new-l }  *)
      Sonata.PUSH (Sonata.Id "!val-val"); Sonata.LOAD; Sonata.UNBIND; Sonata.POP;
      (* v :: f :: left :: stack, { ... env, !loc: l }  *)
      Sonata.PUSH (Sonata.Id "!loc-loc"); Sonata.UNBIND; Sonata.POP;
      (* l :: v :: f :: left :: stack, { ... env }  *)
      Sonata.UNBIND; Sonata.POP;
      (* 3개 복구 *)
      Sonata.CALL
    ]
    )
    (* v :: l :: f :: stack *)
    (* 나머지 cmds를 function으로 만들어야 하고
     * 마지막 명령어는 sonata call
     * 여기서 call할 건 내 꺼
     * 근데 뒤에 놈은 나중에 콜해야하는데?
     * bind를 위해 push f 한 것처럼
     * 앞에 푸시를 해주어야 한다
     * 메모리에 넣었다가 순서 바꿔서 넣고
     *)
  | Sm5.ADD :: cmds -> Sonata.ADD :: (trans' cmds)
  | Sm5.SUB :: cmds -> Sonata.SUB :: (trans' cmds)
  | Sm5.MUL :: cmds -> Sonata.MUL :: (trans' cmds)
  | Sm5.DIV :: cmds -> Sonata.DIV :: (trans' cmds)
  | Sm5.EQ :: cmds -> Sonata.EQ :: (trans' cmds)
  | Sm5.LESS :: cmds -> Sonata.LESS :: (trans' cmds)
  | Sm5.NOT :: cmds -> Sonata.NOT :: (trans' cmds)
  | [] -> []

(* TODO : complete this function *)
let trans : Sm5.command -> Sonata.command = fun command ->
  trans' command
