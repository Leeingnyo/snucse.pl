(*
 * SNU 4190.310 Programming Languages 2018 Fall
 *  K- Interpreter Skeleton Code
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c -> 
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v 
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) = 
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp
    
  type program = exp
  type memory
  type env
  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp

  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
    
  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with
    | Unit -> ()
    | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with
    | Record r -> r
    | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc") 
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    match e with
    | READ x -> 
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | NUM n -> (Num n, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | UNIT -> (Unit, mem)
    | VAR id ->
      let l = lookup_env_loc env id in
      (Mem.load mem l, mem)
    | RECORD fs ->
      if List.length fs = 0 then (Unit, mem)
      else
        let rec calc mem fields values =
          if List.length fields = 0 then (mem, values) else
          let (x, e) = List.hd fields in
          let (v, mem') = eval mem env e in
          calc mem' (List.tl fields) (values @ [v])
          (* 계산만 *)
        in
        let (mem', values) = calc mem fs [] in
        let rec store_mem mem fields values finder =
          if List.length fields = 0 then (finder, mem) else
          let (x, e) = List.hd fields in
          let value = List.hd values in
          let (l, mem') = Mem.alloc mem in
          store_mem (Mem.store mem' l value) (List.tl fields) (List.tl values)
          (* 저장함 *)
          (fun id -> if id = x then l else finder id)
        in
        let (finder, s_mem) = store_mem mem fs values (fun _ -> raise (Error "Unbound")) in
        (Record finder, s_mem)
        (* 바인딩 함수 반환 *)
    | FIELD (e, x) ->
      let (v, mem') = eval mem env e in
      let r = value_record v in
      (Mem.load mem' (r x), mem')
    | ADD (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      (Num (value_int n1 + value_int n2), mem'')
    | SUB (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      (Num (value_int n1 - value_int n2), mem'')
    | MUL (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      (Num (value_int n1 * value_int n2), mem'')
    | DIV (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      (Num (value_int n1 / value_int n2), mem'')
    | EQUAL (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      (try (Bool (v1 = v2), mem'')
        with Invalid_argument exeption -> (Bool false, mem'')
      )
    | LESS (e1, e2) ->
      let (n1, mem') = eval mem env e1 in
      let (n2, mem'') = eval mem' env e2 in
      (Bool (value_int n1 < value_int n2), mem'')
    | NOT e ->
      let (v, mem') = eval mem env e in
      (Bool (not (value_bool v)), mem')
    | ASSIGNF (e1, x, e2) ->
      let (r, mem1) = eval mem env e1 in
      let rv = value_record r in
      let (v, mem2) = eval mem env e2 in
      (v, Mem.store mem2 (rv x) v)
    | SEQ (e1, e2) ->
      let (v1, mem') = eval mem env e1 in
      let (v2, mem'') = eval mem' env e2 in
      (v2, mem'')
    | IF (e, e1, e2) ->
      let (condition, mem') = eval mem env e in
      if value_bool condition = true then
        let (v, mem'') = eval mem' env e1 in
        (v, mem'')
      else
        let (v, mem'') = eval mem' env e2 in
        (v, mem'')
    | WHILE (e1, e2) ->
      let (condition, mem') = eval mem env e1 in
      if value_bool condition = true then
        let (v1, mem1) = eval mem' env e2 in
        let (v2, mem2) = eval mem1 env (WHILE (e1, e2)) in
        (v2, mem2)
      else
        (Unit, mem')
    | LETF (f, x, e1, e2) ->
      eval mem (Env.bind env f (Proc (x, e1, env))) e2
    | CALLV (f, e) ->
      let (id_list, exp, f_env) = lookup_env_proc env f in
      if List.length id_list <> List.length e then raise (Error "InvalidArg")
      else
      let rec calc_args (args, params, mem, pa_env) (* 계산할 남은 거, 메모리 현황 (이미 알록된?) *) =
        if List.length args = 0 then (mem, pa_env)
        else
          let x = List.hd params in
          let e = List.hd args in
          let (v, mem') = eval mem env e in
          let (l, mem'') = Mem.alloc mem' in
          calc_args (List.tl args, List.tl params, Mem.store mem'' l v, Env.bind pa_env x (Addr l))
        (* 아규먼트 파라미터 이름으로 알록하고 *) in
      let (pa_mem, pa_env) = calc_args(e, id_list, mem, f_env) in
      eval (pa_mem) (Env.bind pa_env f (Proc (id_list, exp, f_env))) (exp)
      (* 재귀를 위해 환경에 자기도 넣기 *)
    | CALLR (f, y) ->
      let (id_list, exp, f_env) = lookup_env_proc env f in
      if List.length id_list <> List.length y then raise (Error "InvalidArg")
      else
      let rec calc_args_loc args ids pa_env =
        if List.length args = 0 then pa_env
        else
          let x = List.hd ids in
          let y = List.hd args in
          let l = lookup_env_loc env y in
          calc_args_loc (List.tl args) (List.tl ids) (Env.bind pa_env x (Addr l))
        (* 아규먼트 로케이션을 파라미터 이름으로 바인드하고 *) in
      let pa_env = calc_args_loc y id_list env in
      eval mem (Env.bind pa_env f (Proc (id_list, exp, f_env))) exp
    | _ -> failwith "Unimplemented" (* TODO : Implement rest of the cases *)

  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
end
