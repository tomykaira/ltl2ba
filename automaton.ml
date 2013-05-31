type link = Epsilon of Ltl.ltl option
            | Sigma of Ltl.ltl list * Ltl.ltl option
type node = {
  name: Ltl.FormulaSet.t;
  edges: edge list;
  start: bool;
}
and edge = {
  link : link;
  target: node;
}

(* known_states: add sigma_transformed states *)
let rec reduction_graph  ?start:(start=true) known_states root_set =
  let known_states = root_set :: known_states in
  let is_known state = List.exists ((=) state) known_states in
  let tail_state state = { name = state; edges = []; start = start } in
  let edges = match Ltl.epsilon_transform root_set with
    | None ->
      let (conds, next) = Ltl.sigma_transform root_set in
      if is_known next then
        [{ link = Sigma(conds, None); target = tail_state next }]
      else
        [{ link = Sigma(conds, None); target = reduction_graph known_states next ~start:false }]
    | Some(conv_list) ->
      List.map (fun (set, cond) ->
        if is_known set then
          { link = Epsilon(cond); target = tail_state set }
        else
          { link = Epsilon(cond); target = reduction_graph known_states set ~start:false }
      ) conv_list
  in
  { name = root_set; edges = edges; start = start }

let link_to_string link =
  let format_conds conds =
    BatString.join " ∧ " (List.map Ltl.to_string conds)
  in
  match link with
  | Epsilon(None) -> "ε"
  | Epsilon(Some(formula)) -> "ε, !" ^ (Ltl.to_string(formula))
  | Sigma([], None) -> "Σ"
  | Sigma(conds, None) -> "Σ" ^ (format_conds conds)
  | Sigma([], Some(post)) -> "Σ, !" ^ (Ltl.to_string(post))
  | Sigma(conds, Some(post)) -> "Σ" ^ (format_conds conds) ^ ", !" ^ (Ltl.to_string(post))

let to_graph automaton =
  let set_to_s = Ltl.FormulaSet.to_string in
  let g = ref (Graph.new_graph "Automaton") in
  let rec add_node node =
    let s_name = (set_to_s node.name) in
    g := (if node.start then Graph.add_start else Graph.add_node) !g s_name;
    List.iter (fun {link = link; target = target} ->
      add_node target;
      g := Graph.link !g s_name (set_to_s target.name) (link_to_string link)
    ) node.edges
  in
  add_node automaton;
  !g
