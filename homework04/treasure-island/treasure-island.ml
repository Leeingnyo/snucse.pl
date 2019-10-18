exception IMPOSSIBLE

type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key

type map = End of treasure
  | Guide of string * map
  | Branch of map * map

type implies = ImplyElement | ImplyNode

let rec suggest_temp map current = match map with
  | End t -> (match t with
    | StarBox -> ImplyElement
    | NameBox name -> current
    )
  | Guide (t, m) -> (match suggest_temp m ImplyElement with
    | ImplyElement -> ImplyNode
    | ImplyNode -> raise IMPOSSIBLE
    )
  | Branch (m1, m2) -> (match suggest_temp m1 ImplyNode with
    | ImplyNode -> (match suggest_temp m2 ImplyElement with
      | ImplyElement -> ImplyElement
      | ImplyNode -> raise IMPOSSIBLE
      )
    | ImplyElement -> raise IMPOSSIBLE
    )

let suggest map = match map with
  | End _ -> suggest_temp map ImplyElement
  | Guide (_, _) -> suggest_temp map ImplyNode
  | Branch (_, _) -> suggest_temp map ImplyElement

let getReady map = match map with
  | End t -> (match t with
    | StarBox -> []
    | NameBox name -> []
    )
  | Guide (t, m) -> []
  | Branch (m1, m2) -> []


let test1 = End (NameBox "x") (* [Bar] *)
let test2 = Guide ("x", End (NameBox "x")) (* [Bar] *)
let test3 = Branch (Guide ("x", End (NameBox "x")), End StarBox) (* [Bar] *)
let test4 = Branch (Guide ("x", Branch (End (NameBox "x"), End (NameBox "x"))), End StarBox) (* IMPOSSIBLE *)
let test5 = Branch (Guide ("x", End (NameBox "x")), Branch (Guide ("y", End (NameBox "y")), End StarBox)) (* [Bar] *)
let test6 = Branch (Guide ("x", End (NameBox "x")), Guide ("y", End (NameBox "y"))) (* [Bar, Node (Bar, Bar)] *)
let test7 = Branch (End (NameBox "x"), End StarBox) (* [Bar, Node (Bar, Bar)] *)

let rec key_of_string key = match key with
  | Bar -> "-"
  | Node (key1, key2) -> "(" ^ (key_of_string key1) ^ ", " ^ (key_of_string key2) ^ ")"

let keys_of_string ks = ("[" ^ (List.fold_left (fun s k -> if s = "" then (key_of_string k) else (s ^ ", " ^ (key_of_string k))) "" ks) ^ "]")

let impl_of_string impl = match impl with
  | ImplyElement -> "impl element"
  | ImplyNode -> "impl node"

let impls_of_string impls = ("[" ^ (List.fold_left (fun s k -> if s = "" then (impl_of_string k) else (s ^ ", " ^ (impl_of_string k))) "" impls) ^ "]")

let _ = try (match (suggest test1) with k -> print_endline (impl_of_string k)) with IMPOSSIBLE -> print_endline "impossible"
let _ = try (match (suggest test2) with k -> print_endline (impl_of_string k)) with IMPOSSIBLE -> print_endline "impossible"
let _ = try (match (suggest test3) with k -> print_endline (impl_of_string k)) with IMPOSSIBLE -> print_endline "impossible"
let _ = try (match (suggest test4) with k -> print_endline (impl_of_string k)) with IMPOSSIBLE -> print_endline "impossible"
let _ = try (match (suggest test5) with k -> print_endline (impl_of_string k)) with IMPOSSIBLE -> print_endline "impossible"
let _ = try (match (suggest test6) with k -> print_endline (impl_of_string k)) with IMPOSSIBLE -> print_endline "impossible"
let _ = try (match (suggest test7) with k -> print_endline (impl_of_string k)) with IMPOSSIBLE -> print_endline "impossible"

(*
let rec suggest_temp map current keys = match map with
  | End t -> (match t with
    | StarBox -> (Bar, Bar :: keys)
    | NameBox name -> (current, current :: keys)
    )
  | Guide (t, m) -> (match suggest_temp m Bar keys with (* m 이 암시하는 게 Bar 라서 *)
    | (Node (alpha, beta), added_keys) -> (Node (alpha, beta), added_keys)
    | (Bar, _) -> raise IMPOSSIBLE
    )
  | Branch (m1, m2) -> (match suggest_temp m1 (Node (Bar, Bar)) keys with
    | (Node (m1_alpha, beta), m1_added_keys) -> (match suggest_temp m2 Bar m1_added_keys with
      | (m2_alpha, m2_added_keys) -> (beta, m2_added_keys)
      )
    | (Bar, _) -> raise IMPOSSIBLE
    )

let suggest map = match map with
  | End _ -> suggest_temp map Bar []
  | Guide (_, _) -> suggest_temp map (Node (Bar, Bar)) []
  | Branch (_, _) -> suggest_temp map Bar []
*)

(*
let _ = try (match (suggest test1) with (k, ks) -> print_endline (keys_of_string ks)) with IMPOSSIBLE -> print_endline "impossible"
*)
