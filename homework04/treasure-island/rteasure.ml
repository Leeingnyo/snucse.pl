exception IMPOSSIBLE

type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key

type map = End of treasure
  | Guide of string * map
  | Branch of map * map

(*
getReady: map -> key list
*)
let getReady map = match map with
  | End t -> (match t with
    | StarBox -> []
    | NameBox name -> []
    )
  | Guide (t, m) -> []
  | Branch (m1, m2) -> []

(*********************************************************)

let _ = Random.init 0

(*
map -> graph list -> graph list -> key list
  맵을 보고 변수들의 체인 그래프를 만든다
*)

type exp = VariableBar
  | StarVariable
  | Variable of string
  | TempVariable of string
  | Node of exp * exp

let rec print_exp exp = (match exp with
  | VariableBar -> print_endline "-"
  | TempVariable x -> print_endline x
  | Variable x -> print_endline x
  | StarVariable -> print_endline "*"
  | Node (x, y) -> let _ = print_endline "(" in let _ = print_exp x in let _ = print_exp y in print_endline ")"
)

type graph = exp list list

(*
let test6 = Branch (Guide ("x", End (NameBox "x")), Guide ("y", End (NameBox "y")))
let test6 = Guide ("x", End (NameBox "x"))
let test6 = End (NameBox "x")
let test6 = Branch (Guide ("x", End (NameBox "x")), End (NameBox "x"))
let test6 = Branch (End (NameBox "x"), End StarBox) (* [Bar, Node (Bar, Bar)] *)
*)
let test6 = Guide ("x", Guide ("y", Branch (End (NameBox "x"), Branch (End (NameBox "y"), End (NameBox "x")))))
(*
  analyze_step
    branch
      add Node (alpha, beta), analyze_step m1
      add TempVariable alpha, analyze_step m2
      return TempVariable beta
    guide
      add alpha, Variable string
      add beta, analyze_step m
      return Node (alpha, beta)
    end
      startbox
        add StarVariable, Bar
        return StarVariable
      namebox
        return Variable t
*)

let analyze map =
  let rec add x y graphs =
    let same_temp_in_graph target = List.find (fun element ->
      element == target
    ) in
    (*
    let same_temp_in_graphs target = List.find (fun graph ->
      try let _ = same_temp_in_graph graph in true with Not_found -> false
    ) in
    *)
    let same_var_in_graph target = List.find (fun element ->
      element = target
    ) in
    (*
    let same_var_in_graphs target = List.find (fun graph ->
      try let _ = same_var_in_graph graph in true with Not_found -> false
    ) in
    *)
    let _ = print_exp x in
    let _ = print_exp y in
    let _ = print_endline "--------" in
    if List.length graphs = 0 then
      [x; y] :: graphs
    else (
      try (match x with
        (* x 가 Node 면 *)
        | Node (x1, x2) -> (match y with
          (* y 가 Node 면 *)
          | Node (y1, y2) ->
            let graphs' = add x1 y1 graphs in
            add x2 y2 graphs'
            (* 짝지어 같은 거라 추가해준다 *)
          (* y 가 temp 면 *)
          | TempVariable yv ->
            let same_graph = List.find (fun graph -> try let _ = same_temp_in_graph y graph in true with Not_found -> false) graphs in
            List.map (fun graph -> if (graph = same_graph) then x::graph else graph) graphs
            (* 그래프 중 y와 같은 temp가 있으면 찾아서 더한다. 없으면 [x, y] 를 그래프에 추가한다 *)
          (* y 가 다른 거면 *)
          | _ ->
            let same_graph = List.find (fun graph -> try let _ = same_var_in_graph y graph in true with Not_found -> false) graphs in
            List.map (fun graph -> if (graph = same_graph) then x::graph else graph) graphs
            (* 그래프 중 y 가 같은 걸 찾아서 더한다. 없으면 [x, y] 를 그래프에 추가한다 *)
          )
        (* x 가 temp 면 *)
        | TempVariable av -> (
          try
          let same_graph_x = List.find (fun graph -> try let _ = same_temp_in_graph x graph in true with Not_found -> false) graphs in
          List.map (fun graph -> if (graph = same_graph_x) then y::graph else graph) graphs
          with Not_found -> (
            match y with
              | Node (y1, y2) -> raise Not_found
              | TempVariable yv ->
            let same_graph_y = List.find (fun graph -> try let _ = same_temp_in_graph y graph in true with Not_found -> false) graphs in
            List.map (fun graph -> if (graph = same_graph_y) then x::graph else graph) graphs
              | _ ->
            let same_graph_y = List.find (fun graph -> try let _ = same_var_in_graph y graph in true with Not_found -> false) graphs in
            List.map (fun graph -> if (graph = same_graph_y) then x::graph else graph) graphs
          ))
        (* x 가 다른 것들이면 *)
        | _ ->
          let same_graph = List.find (fun graph -> try let _ = same_var_in_graph x graph in true with Not_found -> false) graphs in
          List.map (fun graph -> if (graph = same_graph) then y::graph else graph) graphs
        )
      with Not_found ->
        (*
        let _ = print_endline "!!! add new graph!" in
        *)
        [x; y] :: graphs
    )
    (*
      graph 중에서 이름이 같은 게 있으면
        x 가 있으면
          y 가 없으면
            y 어펜드
        x 가 없고 y 가 있으면
          x 가 있는 것에
            x 어펜드
      없으면
        graphs 에 [x, y] 어펜드
    *)
  in
  let rec analyze_step map graphs = (match map with
    | Branch (m1, m2) ->
      let alpha = TempVariable ("alpha" ^ (string_of_int (Random.int 10000))) in
      let beta = TempVariable ("beta" ^ (string_of_int (Random.int 10000))) in
      let (child1, graphs') = analyze_step m1 graphs in
      let (child2, graphs'') = analyze_step m2 graphs' in
      let graphs''' = add (Node (alpha, beta)) child1 graphs'' in
      let graphs'''' = add (alpha) child2 graphs''' in
      (beta, graphs'''')
    | Guide (v, m) ->
      let alpha = TempVariable ("alpha" ^ (string_of_int (Random.int 10000))) in
      let beta = TempVariable ("beta" ^ (string_of_int (Random.int 10000))) in
      let (child, graphs') = (analyze_step m graphs) in
      let graphs'' = add alpha (Variable v) graphs' in
      let graphs''' = add beta child graphs'' in
      (Node (alpha, beta), graphs''')
    | End t -> (match t with
      | StarBox ->
        let graphs' = add StarVariable VariableBar graphs in
        (StarVariable, graphs')
      | NameBox v -> (Variable v, graphs)
      )
    )
  in
  let (parent, graphs) = analyze_step map [] in
  add parent (TempVariable "parent") graphs

let graphs = analyze test6
let _ = print_endline "result:"
let _ = List.map (fun x -> (
    let _ = print_endline "----------------------" in
    List.map (fun y ->
      print_exp y
    ) x)
  ) graphs

(*********************************************************)

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

(*
let _ = try (match (getReady test6) with k -> print_endline (keys_of_string k)) with IMPOSSIBLE -> print_endline "impossible"
*)
