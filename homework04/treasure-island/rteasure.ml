exception IMPOSSIBLE

type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key

type map = End of treasure
  | Guide of string * map
  | Branch of map * map

(*
getReady: map -> key list
*)
(*
let getReady map = match map with
  | End t -> (match t with
    | StarBox -> []
    | NameBox name -> []
    )
  | Guide (t, m) -> []
  | Branch (m1, m2) -> []
*)

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
  | NodeVariable of exp * exp

let rec print_exp exp = (match exp with
  | VariableBar -> print_endline "-"
  | TempVariable x -> print_endline x
  | Variable x -> print_endline x
  | StarVariable -> print_endline "*"
  | NodeVariable (x, y) -> let _ = print_endline "(" in let _ = print_exp x in let _ = print_exp y in print_endline ")"
)

type graph = exp list list

(*
let test6 = End (NameBox "x")
let test6 = Guide ("x", End (NameBox "x"))
let test6 = Branch (End (NameBox "x"), End StarBox) (* [Bar, Node (Bar, Bar)] *)
let test6 = Branch (Guide ("x", End (NameBox "x")), End (NameBox "x"))
let test6 = Branch (Guide ("x", End (NameBox "x")), Guide ("y", End (NameBox "y")))
let test6 = Guide ("x", Branch (End (NameBox "y"), Branch (End (NameBox "x"), End StarBox)))
let test6 = Guide ("x", Guide ("y", Branch (End (NameBox "x"), Branch (End (NameBox "y"), End (NameBox "x")))))
let test6 = Guide ("x", Guide ("y", Guide ("z", Branch (Branch (End (NameBox "x"), End (NameBox "y")), End (NameBox "z")))))
let test6 = Branch (Branch (End (NameBox "p"), Branch (End (NameBox "q"), End (StarBox))), Guide ("r", End (NameBox "r")))
let test6 = Branch (Guide ("z", End (NameBox "z")), Guide ("x", Guide ("y", Branch (End (NameBox "x"), Branch (End (NameBox "y"), End (StarBox))))))
let test6 = Branch (Branch (Branch (Guide ("t", Guide ("o", Branch (End (NameBox "o"), End (NameBox "t")))), Guide ("h", End (NameBox "h"))), Guide ("f", End (NameBox "f"))), End (NameBox "v"))
let test6 = Branch (Branch (Branch (Guide ("x", Guide ("y", Guide ("z", Branch (Branch (End (NameBox "x"), End (NameBox "y")), End (NameBox "z"))))), End (NameBox "a")), End (NameBox "b")), End (NameBox "c"))
let test6 = Branch (End (NameBox "z"), Guide ("x", Branch (Guide ("y", Branch (End (NameBox "x"), End (NameBox "y"))), End StarBox)))

let test6 = Branch (End StarBox, End StarBox)
let test6 = Branch (End (NameBox "x"), End (NameBox "x"))
let test6 = Guide ("x", Branch (End (StarBox), End (NameBox "x")))
let test6 = Branch (Guide ("x", Branch (End (NameBox "x"), End (NameBox "x"))), End (StarBox))
let test6 = Branch (Branch (End (NameBox "q"), End (NameBox "p")), Guide ("q", Branch (End (NameBox "p"), End (NameBox "q"))))
*)
let test6 = End (NameBox "x")
let test6 = Branch (Branch (Branch (Guide ("t", Guide ("o", Branch (End (NameBox "o"), End (NameBox "t")))), Guide ("h", End (NameBox "h"))), Guide ("f", End (NameBox "f"))), End (NameBox "v"))
(*
  analyze_step
    branch
      add NodeVariable (alpha, beta), analyze_step m1
      add TempVariable alpha, analyze_step m2
      return TempVariable beta
    guide
      add alpha, Variable string
      add beta, analyze_step m
      return NodeVariable (alpha, beta)
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
    (*
    let _ = print_exp x in
    let _ = print_exp y in
    let _ = print_endline "--------" in
    *)
    if List.length graphs = 0 then
      [x; y] :: graphs
    else (
      try (match x with
        (* x 가 Node 면 *)
        | NodeVariable (x1, x2) -> (match y with
          (* y 가 Node 면 *)
          | NodeVariable (y1, y2) ->
            let graphs' = add x1 y1 graphs in
            add x2 y2 graphs'
            (* 짝지어 같은 거라 추가해준다 *)
          (* y 가 temp 면 *)
          | TempVariable yv ->
            let same_graph = List.find (fun graph -> try let _ = same_temp_in_graph y graph in true with Not_found -> false) graphs in
            (* y 가 있는 그래프에 node 들이 있으면 내가 노드니까 짝지은 것을 추가해준다 *)
            let nodes = List.filter (fun e -> match e with NodeVariable (_, _) -> true | _ -> false) same_graph in
            let graphs' = List.fold_left (fun graphs -> fun node ->
              match node with
              | NodeVariable (n1, n2) -> let graphs' = add x1 n1 graphs in
                add x2 n2 graphs'
              | _ -> graphs
            ) graphs nodes in
            List.map (fun graph -> if (graph = same_graph) then x::graph else graph) graphs'
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
            (* x가 있으면 거기에 y 도 있는지 확인하고 없으면 추가함 *)
          if (try let _ = same_var_in_graph y same_graph_x in true with Not_found -> false) then graphs else
            let graphs' =
            (match y with
            (* y 가 노드면 *)
            | NodeVariable (y1, y2) ->
              let nodes = List.filter (fun e -> match e with NodeVariable (_, _) -> true | _ -> false) same_graph_x in
              (* 그 그래프에 있는 노드들 *)
              List.fold_left (fun graphs -> fun node ->
                match node with
                | NodeVariable (n1, n2) -> let graphs' = add y1 n1 graphs in
                  add y2 n2 graphs'
                | _ -> graphs
                ) graphs nodes
            | _ -> graphs
            ) in
            (* 사실 옳게 한 건지 모르겠다 *)
            List.map (fun graph -> if (graph = same_graph_x) then y::graph else graph) graphs'
          with Not_found -> (
            match y with
              | NodeVariable (_, _) -> raise Not_found
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
          if (try let _ = same_var_in_graph y same_graph in true with Not_found -> false) then graphs else
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
      let graphs''' = add (NodeVariable (alpha, beta)) child1 graphs'' in
      let graphs'''' = add (alpha) child2 graphs''' in
      (beta, graphs'''')
    | Guide (v, m) ->
      let alpha = TempVariable ("alpha" ^ (string_of_int (Random.int 10000))) in
      let beta = TempVariable ("beta" ^ (string_of_int (Random.int 10000))) in
      let (child, graphs') = (analyze_step m graphs) in
      let graphs'' = add alpha (Variable v) graphs' in
      let graphs''' = add beta child graphs'' in
      (NodeVariable (alpha, beta), graphs''')
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
let print_graphs graphs =
  let _ = print_endline "--------------" in
  let _ = print_endline "result:" in
  let _ = List.map (fun x -> (
    let _ = print_endline "------------" in
    List.map (fun y ->
      print_exp y
    ) x)
  ) graphs in
  print_endline "--------------"

(*
  StarVariable Variable 없는 것들로
*)
let trim_graphs graphs =
  let has_node graph =
    try
    let _ = List.find
      (fun element -> match element with
      | NodeVariable (_, _) -> true
      | _ -> false
      )
      graph in
    true
    with Not_found -> false
  in
  let has_variable graph =
    try
    let _ = List.find
      (fun element -> match element with
      | Variable _ | StarVariable -> true
      | _ -> false
      )
      graph in
    true
    with Not_found -> false
  in
  let sort_grpahs graphs =
    List.sort
    (
      fun graph1 -> fun graph2 ->
      if (has_variable graph1 && has_variable graph2) then 0
      else if (has_variable graph1) then 1
      else if (has_variable graph2) then -1
      else 0
    )
    graphs
  in
  let sorted_graphs = sort_grpahs graphs in
  (*
    그래프들 중
    어떤 그래프가 변수도 없고 노드도 없으면
      하나를 프리베리어블로 잡고
      다른 그래프들을 순회해서 프리베리어블로 잡은 거 뺴고 나머지가 있는 경우
      프리베리어블로 교체된 다른 그래프들로 만들고
      자기는 뺌
  *)
  let (free_variables, graphs) =
    List.fold_left
    (fun (free_variables, graphs) -> fun current ->
      if (not (has_variable current) && not (has_node current)) then
        let fv = List.nth current 0 in
        let nfv = List.filter (fun e -> e != fv) current in
        (
          fv::free_variables,
          List.map
          (fun graph ->
            List.map
            (fun e_in_g ->
              match  e_in_g with
                | NodeVariable (e1, e2) -> (match
                  (
                    (try let _ = List.find (fun e_in_nfv -> e_in_nfv == e1) nfv in true with Not_found -> false),
                    (try let _ = List.find (fun e_in_nfv -> e_in_nfv == e2) nfv in true with Not_found -> false)
                  )
                  with
                  | (true, true) -> NodeVariable (fv, fv)
                  | (true, false) -> NodeVariable (fv, e2)
                  | (false, true) -> NodeVariable (e1, fv)
                  | _ -> e_in_g
                )
                | _ ->
                  if (try let _ = List.find (fun e_in_nfv -> e_in_nfv == e_in_g) nfv in true with Not_found -> false) then fv else e_in_g
            )
            graph
          )
          (
            List.filter
            (fun graph -> graph <> current) (* <> 를 != 로 해야하나? 고민의 여지 *)
            graphs
          )
        )
      else
      (free_variables, graphs)
    )
    ([], sorted_graphs)
    sorted_graphs
  in
  let make_free graphs free_variables =
    List.map
    (fun graph ->
      List.map
      (fun e_in_g ->
        match e_in_g with
          | NodeVariable (e1, e2) ->
            (match (
              (try let _ = List.find (fun fv -> fv == e1) free_variables in true with Not_found -> false),
              (try let _ = List.find (fun fv -> fv == e2) free_variables in true with Not_found -> false)
            )
            with
              | (true, true) -> NodeVariable (VariableBar, VariableBar)
              | (true, false) -> NodeVariable (VariableBar, e2)
              | (false, true) -> NodeVariable (e1, VariableBar)
              | _ -> e_in_g
            )
          | _ ->
            if (try let _ = List.find (fun fv -> fv == e_in_g) free_variables in true with Not_found -> false) then VariableBar else e_in_g
      )
      graph
    )
    graphs
  in
  make_free graphs free_variables

let check_impossible graphs =
  try
  let _ = List.find (fun graph ->
    (*
      안되는 조건
      cycle 이 있는가
      * 가 있는데 NODE 가 있다
    *)
    let has_cycle graph =
      let has_cycle_with target graph =
        (match target with
        | TempVariable _ ->
        (try
        let _ = List.find
          (fun x -> match x with
          | NodeVariable (x1, x2) -> x1 == target || x2 == target
          | _ -> false
          )
        graph
        in true
        with Not_found -> false)
        | _ -> false)
      in
      try
      let _ = List.find (fun x -> has_cycle_with x graph) graph
      in true
      with Not_found -> false
    in
    let has_star_with_node graph =
      let has_node graph = try let _ = List.find (fun e -> match e with NodeVariable (_, _) -> true | _ -> false) graph in true with Not_found -> false in
      let has_star graph = try let _ = List.find (fun e -> match e with StarVariable -> true | _ -> false) graph in true with Not_found -> false in
      has_star graph && has_node graph
    in
    has_cycle graph || has_star_with_node graph
  ) graphs
  in
  raise IMPOSSIBLE
  with Not_found -> graphs

let merge_graphs graphs =
  List.fold_left
  (fun result -> fun graph ->
    try
    let selected_graph = List.find (fun already ->
      List.fold_left (fun result -> fun element ->
        result ||
        (try
        let _ = 
        List.find (fun ae -> ae == element) already
        in true
        with Not_found -> false)
      ) false graph
    ) result in
    (* 그래프의 TempVariable 가 이미 result 의 그래프 중 하나에 있다면 *)
    List.map
    (fun rg ->
      if (rg = selected_graph) then
        List.fold_left
        (fun al -> fun n ->
          try let _ = List.find (fun x -> x = n) al in al
          with Not_found -> n::al
        )
        []
        (List.rev_append rg graph)
      else rg
    )
    result
    with Not_found -> graph::result
  )
  []
  graphs

let trimmed_graphs = (check_impossible (trim_graphs (merge_graphs graphs)))

let resolve graphs =
  (*
    그래프들 중 variable 을 갖고 있는 그래프들을 본다
    구석구석 (노드도 포함해서)
    temp 를 만나면 (이 temp 를 first, current 라고 하자)
    다른 그래프들에 그 temp 가 있는지 보고
    그 그래프가 node나 -를 갖고 있는지 찾아본다
    node 에 또 temp 가 있으면 또 찾는다 (대신 first 와 같으면 사이클이니 IMPOSSIBLE)
    이렇게 하나씩 해나간다
    다른 그래프들에 그 temp 가 없으면 - 를 넣는다
  *)
  let has_temp_variable temp graph =
    try let _ = List.find (fun e -> e = temp) graph in true with Not_found -> false
  in
  let has_var graph =
    try let _ = List.find (fun e -> match e with Variable _ -> true | _ -> false) graph in true with Not_found -> false
  in
  let has_bar graph =
    try let _ = List.find (fun e -> match e with VariableBar -> true | _ -> false) graph in true with Not_found -> false
  in
  let has_node graph =
    try let _ = List.find (fun e -> match e with NodeVariable (_, _) -> true | _ -> false) graph in true with Not_found -> false
  in
  (*
  let find_node_with target graph =
    target
  in
  *)
  let has_variable graph =
    try let _ = List.find (fun e -> match e with Variable _ | StarVariable -> true | _ -> false) graph
    in true
    with Not_found -> false
  in
  let target_graphs = List.filter (fun graph -> has_variable graph) graphs in
  List.map
  (fun target_graph ->
    let rec transform element = match element with
      | NodeVariable (e1, e2) -> NodeVariable (transform e1, transform e2)
      | TempVariable _ -> (
        try let another_grpah = List.find (fun graph -> graph <> target_graph && (has_temp_variable element graph)) graphs in
        (* 다른 그래프에서 temp 가 있는 그래프를 찾는다 *)
        (* 다른 그래프에 - 가 있으면 - *)
        if (has_bar another_grpah) then VariableBar
        (* 변수가 있으면 변수를 *)
        else if (has_var another_grpah) then
          List.find (fun e -> match e with Variable _ -> true | _ -> false) another_grpah
        (* 없고 노드가 있으면 노드를 넣고 또 돌림 *)
        else if (has_node another_grpah) then
          transform (List.find (fun e -> match e with NodeVariable (_, _) -> true | _ -> false) another_grpah)
        else element
        with Not_found -> element
        (* 없으면 - *)
        )
      | _ -> element
    in
    List.map transform target_graph
  )
  target_graphs

(* 무한루프 디버깅
let _ = print_endline "-"
let _ = print_graphs (trimmed_graphs)
let _ = print_graphs (resolve trimmed_graphs)
*)
let formula = (resolve trimmed_graphs)

let make_key formula =
  (*  *)
  let keys =
    try
    let resolve_step_setting_first graph =
      let first = graph in
      let rec resolve_step graph =
        let rec value_of e = match e with
          | NodeVariable (x1, x2) -> value_of x1 + value_of x2 (* stack overflow 문제 가능성 *)
          | _ -> 1
        in
        (*
        let has_only_one graph = List.for_all (fun e -> value_of e = 1) graph in
        if (has_only_one graph) then Bar
        else
        *)
        let complicates = List.filter (fun x -> value_of x <> 1) graph in
        if List.length complicates = 0 then Bar
        else
        let rec resolve_exp exp = (match exp with
          | NodeVariable (m1, m2) ->
            Node (resolve_exp m1, resolve_exp m2)
          | Variable _ ->
            let variable_graph = List.find (fun graph -> try let _ = List.find (fun ele -> ele = exp) graph in true with Not_found -> false) formula in
            (* exp 를 갖는 그래프를 찾음 *)
            if (first == variable_graph) then raise IMPOSSIBLE else
            (* 그게 first 와 같으면 raise *)
            resolve_step variable_graph
          | _ -> Bar
          )
        in
        let transformed = List.map (fun exp -> resolve_exp exp) complicates in
        List.fold_left (fun answer -> fun target -> if (answer = target) then answer else raise IMPOSSIBLE) (List.nth transformed 0) transformed
      in
      resolve_step graph
    in
    List.map
    resolve_step_setting_first
    formula
    with Stack_overflow -> raise IMPOSSIBLE
  in
  (* 중복 제거 *)
  List.fold_left
  (fun keys -> fun target_key ->
    try let _ = List.find (fun key -> key = target_key) keys in keys
    with Not_found -> target_key::keys
  ) [] keys

let getReady map =
  make_key (resolve (check_impossible (trim_graphs (merge_graphs (analyze map)))))

let rec key_of_string key = match key with
  | Bar -> "-"
  | Node (key1, key2) -> "(" ^ (key_of_string key1) ^ ", " ^ (key_of_string key2) ^ ")"

let keys_of_string ks = ("[" ^ (List.fold_left (fun s k -> if s = "" then (key_of_string k) else (s ^ ", " ^ (key_of_string k))) "" ks) ^ "]")

let _ = try (match (getReady test6) with k -> print_endline (keys_of_string k)) with IMPOSSIBLE -> print_endline "impossible"

(*********************************************************)

(*
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
*)

(*
let _ = try (match (getReady test6) with k -> print_endline (keys_of_string k)) with IMPOSSIBLE -> print_endline "impossible"
*)
