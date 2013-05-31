type link = Epsilon of Ltl.ltl option
            | Sigma of Ltl.ltl list * Ltl.ltl option
type state = Ltl.FormulaSet.t
type transition = {
  link : link;
  s: state;
  t: state;
}
type automaton = {
  starts: state list;
  finals: state list;
  transitions: transition list;
}

(* known_states: add sigma_transformed states *)
let rec reduction_graph  transitions state =
  let is_known trans = List.exists ((=) trans) transitions in
  match Ltl.epsilon_transform state with
    | None ->
      let (conds, next) = Ltl.sigma_transform state in
      let trans = { link = Sigma(conds, None); s = state; t = next } in
      if is_known trans then
        transitions
      else
        reduction_graph (trans :: transitions) trans.t
    | Some(conv_list) ->
      List.fold_left (fun transitions (next, cond) ->
        let trans = { link = Epsilon(cond); s = state; t = next } in
        if is_known trans then
          transitions
        else
          reduction_graph (trans :: transitions) trans.t
      ) transitions conv_list

let construct_from start_state =
  { starts = [start_state]; finals = []; transitions = reduction_graph [] start_state }

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
  let g = (Graph.new_graph "Automaton") in
  let is_start s = List.exists ((=) s) automaton.starts in
  let add_node_function s = (if is_start s then Graph.add_start else Graph.add_node) in
  List.fold_left (fun g { link = link; s = s; t = t } ->
    let s_string = set_to_s s in
    let t_string = set_to_s t in
    let g = (add_node_function s) g s_string in
    let g = (add_node_function t) g t_string in
    Graph.link g s_string t_string (link_to_string link)
  ) g automaton.transitions
