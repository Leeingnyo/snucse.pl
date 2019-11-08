(*
 * SNU 4190.310 Programming Languages 
 * Homework "SM5 Garbage Collector" Solution
 * DongKwon Lee (dklee@ropas.snu.ac.kr)
 *)

module type SM5 = 
sig
  type record
  type loc 
  type value = Z of int | B of bool | L of loc | Unit | R of record
  type cmd = 
    | PUSH of obj 
    | POP 
    | STORE 
    | LOAD 
    | JTR of command * command
    | MALLOC 
    | BOX of int 
    | UNBOX of string 
    | BIND of string 
    | TRYSTART
    | TRYEND
    | UNBIND
    | GET 
    | PUT 
    | CALL 
    | ADD 
    | SUB 
    | MUL 
    | DIV
    | EQ
    | LESS
    | NOT
  and obj = Val of value | Id of string | Fn of string * command
  and command = cmd list

  exception GC_Failure
  exception Error of string

  val debug_mode : bool ref
  val gc_mode : bool ref

  val command_to_str : string -> command -> string
  val run : command -> unit

end

module Sm5 : SM5 =
struct

  (* Refer to the document http://ropas.snu.ac.kr/~kwang/4190.310/17/hw5.pdf for
     SM5's syntax, domain and semantic rules 
  *)

  type loc = int * int
  type record = (string * loc) list
  type value = Z of int | B of bool | L of loc | Unit | R of record
  type cmd = 
    | PUSH of obj 
    | POP 
    | STORE 
    | LOAD 
    | JTR of command * command
    | MALLOC 
    | BOX of int 
    | UNBOX of string 
    | BIND of string 
    | TRYSTART
    | TRYEND
    | UNBIND
    | GET 
    | PUT 
    | CALL 
    | ADD 
    | SUB 
    | MUL 
    | DIV 
    | EQ 
    | LESS 
    | NOT
  and obj = Val of value | Id of string | Fn of string * command
  and command = cmd list

  type proc = string * command * environment
  and evalue = Loc of loc | Proc of proc
  and environment = (string * evalue) list

  type svalue = V of value | P of proc | M of (string * evalue)
  type stack = svalue list
  type memory = (loc * value) list
  type continuation = (command * environment) list

  type tryscope = (command * stack * environment * continuation) list
  (* h 번역한 것, 당시 스택, 당시 환경, 당시 컨티뉴에이션 *)

  exception GC_Failure
  exception Error of string

  let debug_mode = ref false

  let gc_mode = ref false

  let loc_to_str (base, offset) = 
    Printf.sprintf "<%d, %d>" base offset

  let val_to_str = function 
    | Z i -> string_of_int i
    | B b -> string_of_bool b
    | R pair_list -> 
      let pair_to_str (s, l) = Printf.sprintf "(%s, %s)" s (loc_to_str l) in
      "{" ^  (String.concat ", " (List.map pair_to_str pair_list)) ^ "}"
    | Unit -> "unit"
    | L l -> loc_to_str l

  let rec cmd_to_str indent cmd = 
    match cmd with
    | PUSH obj -> Printf.sprintf "push %s" (obj_to_str indent obj)
    | POP -> "pop"
    | STORE -> "store"
    | LOAD -> "load"
    | JTR (c1, c2) -> 
      let c1_str = (command_to_str ("  " ^ indent) c1) in
      let c2_str = (command_to_str ("  " ^ indent) c2) in
      Printf.sprintf "jtr (\n%s,\n%s)" c1_str c2_str
    | MALLOC -> "malloc"
    | BOX z -> Printf.sprintf "box %d" z
    | UNBOX x -> Printf.sprintf "unbox %s" x
    | BIND x -> Printf.sprintf "bind %s" x
    | UNBIND -> "unbind"
    | TRYSTART -> "trystart"
    | TRYEND -> "tryend"
    | GET -> "get"
    | PUT -> "put"
    | CALL -> "call"
    | ADD -> "add"
    | SUB -> "sub"
    | MUL -> "mul"
    | DIV -> "div"
    | EQ -> "eq"
    | LESS -> "less"
    | NOT -> "not"
  and obj_to_str indent obj = 
    match obj with
    | Val v -> val_to_str v
    | Id x -> x
    | Fn (f, comm) -> 
      let new_indent = "  " ^ indent in
      let comm_str = command_to_str new_indent comm in
      Printf.sprintf "(%s,\n%s\n%s)" f comm_str new_indent
  and command_to_str indent comm = 
    indent ^ "[" ^ 
    (String.concat (";\n" ^ indent) (List.map (cmd_to_str indent) comm)) ^ "]"
  
  let rec proc_to_str indent (x, comm, env) =
    let new_indent = ("  " ^ indent) in
    let comm_str = command_to_str new_indent comm in
    let env_str = env_to_str new_indent env in
    Printf.sprintf "(fun %s ->\n%s\n%s), Env =\n%s" x comm_str new_indent env_str
  and evalue_to_str indent ev =
    match ev with
    | Loc l -> loc_to_str l
    | Proc p -> proc_to_str indent p
  and env_to_str indent env =
    let entry_to_str (x, ev) = 
      Printf.sprintf "%s : %s" x (evalue_to_str indent ev)
    in
    indent ^ "[" ^ 
    (String.concat (";\n" ^ indent) (List.map entry_to_str env)) ^ "]"

  let sval_to_str = function
    | V v -> val_to_str v
    | P p -> proc_to_str "" p
    | M (x, ev) -> Printf.sprintf "(%s, %s)" x (evalue_to_str "" ev)

  let stack_to_str s =
    "[" ^ (String.concat ";\n" (List.map sval_to_str s)) ^ "]"

  let mem_to_str m =
    let entry_to_str (l, v) =
      Printf.sprintf "%s : %s" (loc_to_str l) (val_to_str v)
    in
    String.concat "\n" (List.map entry_to_str m)
  
  let cont_to_str k = 
    let entry_to_str (comm, env) = 
      let comm_str = command_to_str "  " comm in
      let env_str = env_to_str "  " env in
      Printf.sprintf "Command :\n%s\nEnv :\n%s" comm_str env_str
    in
    String.concat "\n----------\n" (List.map entry_to_str k)

  let tryscope_to_str t =
    let entry_to_str (comm, s, env, k) =
      let comm_str = command_to_str "  " comm in
      let env_str = env_to_str "  " env in
      let s_str = stack_to_str s in
      let cont_str = cont_to_str k in
      Printf.sprintf "Command :\n%sStack :\n%sEnv :\n%sContnuation :\n%s" comm_str s_str env_str cont_str
    in
    String.concat "\n----------\n" (List.map entry_to_str t)

  let lookup_env x e =
    try List.assoc x e with Not_found -> raise (Error ("Unbound name : " ^ x))

  let lookup_record x r = 
    try List.assoc x r with Not_found -> raise (Error ("Unfound field : " ^ x))

  let load l m = 
    try List.assoc l m with 
    Not_found -> raise (Error ("Uninitialized location : %s" ^ (loc_to_str l)))

  let store l v m = 
    if List.mem_assoc l m then 
      (l, v) :: (List.remove_assoc l m) 
    else (l, v) :: m

  let mem_limit = 128

  let loc_id = ref 0

  let reachable_locs : (loc list) ref = ref []
  
  (* TODO : Complete this function.
   * Implement the GC algorithm introduced in class material.
   *)
  let malloc_with_gc s m e c k =
    if List.length m < mem_limit then
      let _ = loc_id := !loc_id + 1 in
      ((!loc_id, 0), m)
    else
      let _ = reachable_locs :=
        let rec collect env reachable_locs =
          (* env 를 탐색해서 수집한다 *)
          List.fold_left (fun reachable_locs -> (fun element -> match element with
            (* 수집 탱크, 수집할 건지 확인 할 것 *)
            (* Loc 이면 *)
            | (_, Loc loc) ->
              (* 메모리가 loc 이면 걔도 수집해야함 *)
              let rec chain loc reachable_locs =
                (* chain 은 뭐냐면 메모리에서 loc 가리키는 변수가 loc 관련이면 걔도 수집해야함
                 * loc 관련이면 걔도 또 검사하러 가야하고
                 * 아니면 loc 만 수집하고 여기서 마치겠습니다
                 *)
                let apply_offset loc = match loc with
                  | (base, offset) -> base
                in
                let same_base loc =
                  List.map
                  (fun e -> match e with
                  | (l, v) -> l
                  )
                  (
                  List.find_all
                  (fun e -> match (e, loc) with
                  | (((b_e, _), _), (base, _)) -> (base = b_e)
                  )
                  m
                  )
                in

                let value =
                  List.find
                  (fun e -> match e with
                  | (l, v) -> apply_offset l = apply_offset loc
                  )
                  m
                in
                (
                let reachable_locs' = loc :: List.flatten [reachable_locs; (same_base loc)] in
                match value with
                | (_, L _loc) -> chain _loc reachable_locs'
                | (_, R record) -> (
                  List.fold_left
                  (fun reachable_locs -> fun field -> match field with
                  | (_, _loc) -> chain _loc reachable_locs
                  )
                  reachable_locs'
                  record
                )
                | _ -> reachable_locs'
                )
              in
              chain loc reachable_locs
              (* 일단 수집하고 *)
            (* Proc 이면 *)
            | (_, Proc proc) -> (match proc with
              | (_, _, proc_env) -> List.flatten [reachable_locs; collect proc_env []]
              (* 함수의 env 를 탐색 *)
            )
          ))
          reachable_locs
          env
        in
        List.fold_left
        (fun reachable_locs -> fun continuation -> match continuation with
          (* 수집 탱크, 컨티뉴에이션 *)
          | (_, con_env) -> List.flatten [reachable_locs; collect con_env []]
          (* 컨티뉴에이션의 인바에서 콜렉트한 것과 합친다 *)
        )
        (collect e []) (* 현재 환경에서 수집 *)
        k
      in
      (*
       * 할 것... 현재 e 에 있는 것들, e 의 함수에 있는 것들,
       *)
      (* TODO : Add the code that marks the reachable locations.
       * let _ = ... 
       *)
      let new_m = List.filter (fun (l, _) -> List.mem l !reachable_locs) m in
      if List.length new_m < mem_limit then
        let _ = loc_id := !loc_id + 1 in
        let new_l = (!loc_id, 0) in
        (new_l, new_m)
      else
        raise GC_Failure

  let malloc () = 
    let _ = loc_id := !loc_id + 1 in
    (!loc_id, 0)

  let rec box_stack s i accum_list =
    if i = 0 then (V (R accum_list)) :: s else
      match s with
      | M (x, Loc l) :: s' -> box_stack s' (i - 1) ((x, l) :: accum_list)
      | _ -> raise (Error "Box() : entry is not (x, loc) pair")

  let is_equal v1 v2 =
    match v1, v2 with
    | Z z1, Z z2 -> z1 = z2
    | B b1, B b2 -> b1 = b2
    | R r1, R r2 -> 
      List.sort Pervasives.compare r1 = List.sort Pervasives.compare r2
    | Unit, Unit -> true
    | L (base1, z1), L (base2, z2) ->
      base1 = base2 && z1 = z2
    | _ , _ -> false

  let rec step = function
    | (s, m, e, PUSH (Val v) :: c, k, t) -> (V v :: s, m, e, c, k, t)
    | (s, m, e, PUSH (Id x) :: c, k, t) ->
      (match lookup_env x e with
      | Loc l -> (V (L l) :: s, m, e, c, k, t)
      | Proc p -> (P p :: s, m, e, c, k, t))
    | (s, m, e, PUSH (Fn (x, c')) :: c, k, t) -> (P (x, c',e)::s, m, e, c, k, t)
    | (w :: s, m, e, POP :: c , k, t) -> (s, m, e, c, k, t)
    | (V (L l) :: V v :: s, m, e, STORE :: c, k, t) -> (s, store l v m, e, c, k, t)
    | (V (L l) :: s, m, e, LOAD :: c, k, t) -> (V (load l m) :: s, m, e, c, k, t)
    | (V (B b)::s, m, e, JTR (c1,c2) :: c, k, t) -> 
      (s, m, e, (if b then c1 @ c else (c2 @ c)), k, t)
    | (s, m, e, MALLOC :: c, k, t) -> 
      if !gc_mode then
        let (new_l, new_m) = malloc_with_gc s m e c k in
        (V (L new_l) :: s, new_m, e, c, k, t)
      else 
        (V (L (malloc ())) :: s, m, e, c, k, t)
    | (s, m, e, BOX z :: c, k, t) ->
      if z <= 0 then 
        raise (Error "Box(n): non-positive n") 
      else 
        (box_stack s z [], m, e, c, k, t)
    | (V (R r) :: s, m, e, UNBOX x :: c, k, t) -> 
      (V (L (lookup_record x r)) :: s, m, e, c, k, t)
    | (V (L l) :: s, m, e, BIND x :: c, k, t) -> (s, m, (x, Loc l) :: e, c, k, t)
    | (P p :: s, m, e, BIND x :: c, k, t) -> (s, m, (x, Proc p) :: e, c, k, t)
    | (s, m, (x, ev) :: e, UNBIND :: c, k, t) -> (M (x, ev) :: s, m, e, c, k, t)
    | (V (L l) :: V v :: P (x, c', e') :: s, m, e, CALL :: c, k, t) ->
      (s, store l v m, (x, Loc l) :: e', c', (c, e) :: k, t)
    | (s, m, e, [], (c, e') :: k, t) -> (s, m, e', c, k, t)
    | (s, m, e, GET :: c, k, t) -> (V (Z (read_int())) :: s, m, e, c, k, t)
    | (V (Z z) :: s, m, e, PUT :: c, k, t) -> 
      let _ = print_endline (string_of_int z) in
      (s, m, e, c, k, t)
    | (V (Z z2) :: V (Z z1) :: s, m, e, ADD :: c, k, t) ->
      (V (Z (z1 + z2))::s, m, e, c, k, t)
    | (V (Z z) :: V (L (base, offset)) :: s, m, e, ADD :: c, k, t)
    | (V (L (base, offset)) :: V (Z z) :: s, m, e, ADD :: c, k, t) -> 
      if offset + z >= 0 then
        (V (L (base, offset + z)) :: s, m, e, c, k, t) 
      else 
        raise (Error "Negative loc offset spawned")
    | (V (Z z2) :: V (Z z1) :: s, m, e, SUB :: c, k, t) ->
      (V (Z (z1 - z2)) :: s, m, e, c, k, t)
    | (V (Z z) :: V (L (base, offset)) :: s, m, e, SUB :: c, k, t) -> 
      if offset - z >= 0 then
        (V (L (base, offset - z)) :: s, m, e, c, k, t) 
      else  
        raise (Error "Negative loc offset spanwed")
    | (V (Z z2) :: V (Z z1) :: s, m, e, MUL :: c, k, t) -> 
      (V (Z (z1 * z2)) :: s, m, e, c, k, t)
    | (V (Z z2) :: V (Z z1) :: s, m, e, DIV :: c, k, t) -> 
      (V (Z (z1 / z2)) :: s, m, e, c, k, t)
    | (V v2 :: V v1 :: s, m, e, EQ :: c, k, t) -> 
      (V (B (is_equal v1 v2)) :: s, m, e, c, k, t)
    | (V (Z z2) :: V (Z z1) :: s, m, e, LESS :: c, k, t) -> 
      (V (B (z1 < z2)) :: s, m, e, c, k, t)
    | (V (B b) :: s, m, e, NOT :: c, k, t) -> (V (B (not b)) :: s, m, e, c, k, t)
    | s, m, e, c, k, t ->
      raise (Error "Invalid machine state")

  let rec run_helper (s, m, e, c, k, t) = 
    match c, k with
    | [], [] -> ()
    | _ -> 
      let _ = if !debug_mode then
        (print_endline "====== Machine state ======";
        print_newline();
        print_endline "***** Command *****";
        print_endline (command_to_str "" c);
        print_newline();
        print_endline "***** Stack *****";
        print_endline (stack_to_str s);
        print_newline();
        print_endline "***** Environment *****";
        print_endline (env_to_str "" e);
        print_newline();
        print_endline "***** Memory *****";
        print_endline (mem_to_str m);
        print_newline();
        print_endline "***** Continuation *****";
        print_endline (cont_to_str k);
        print_newline();
        print_endline "***** Try Scope *****";
        print_endline (tryscope_to_str t);
        print_newline())
      in
      run_helper (step (s, m, e, c, k, t))

  let run c = run_helper ([], [], [], c, [], [])

end
