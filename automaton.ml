type link = Epsilon of Ltl.ltl option
            | Sigma of Ltl.ltl option * Ltl.ltl option
type node = {
  name: Ltl.FormulaSet.t;
  edges: edge list;
}
and edge = {
  link : link;
  target: node;
}

let leaf_node set =
  { name = set; edges = [] }

let rec reduction_graph root_set =
  let edges = match Ltl.epsilon_transform root_set with
    | None -> []
    | Some(conv_list) ->
      List.map (fun (set, cond) -> { link = Epsilon(cond); target = reduction_graph set }) conv_list
  in
  { name = root_set; edges = edges }

let link_to_string = function
  | Epsilon(None) -> "ε"
  | Epsilon(Some(formula)) -> "ε, !" ^ (Ltl.to_string(formula))
  | Sigma(None, None) -> "Σ"
  | Sigma(Some(cond), None) -> "Σ" ^ (Ltl.to_string(cond))
  | Sigma(None, Some(post)) -> "Σ, !" ^ (Ltl.to_string(post))
  | Sigma(Some(cond), Some(post)) -> "Σ" ^ (Ltl.to_string(cond)) ^ ", !" ^ (Ltl.to_string(post))

let to_graph automaton =
  let set_to_s = Ltl.FormulaSet.to_string in
  let g = ref (Graph.new_graph "Automaton") in
  let rec add_node node =
    let s_name = (set_to_s node.name) in
    g := Graph.add_node !g s_name;
    List.iter (fun {link = link; target = target} ->
      add_node target;
      g := Graph.link !g s_name (set_to_s target.name) (link_to_string link)
    ) node.edges
  in
  add_node automaton;
  !g
