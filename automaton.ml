type link = Epsilon of Ltl.ltl option
            | Sigma of Ltl.ltl list * Ltl.ltl option
type state = Ltl.FormulaSet.t
type transition = {
  link : link;
  s: state;
  t: state;
}
module OrderedTransition = struct
  type t = transition
  let compare a b =
    let c = compare a.link b.link in
    if c <> 0 then
      c
    else
      let c = Ltl.FormulaSet.compare a.s b.s in
      if c <> 0 then
        c
      else
        Ltl.FormulaSet.compare a.t b.t
  let (=) a b =
    compare a b = 0
end

module TransitionSet = ExtendedSet.Make(OrderedTransition)

type automaton = {
  starts: state list;
  finals: state list;
  transitions: TransitionSet.t;
}

let link_to_string link =
  let format_conds conds =
    BatString.join " ∧ " (List.map Ltl.to_string conds)
  in
  match link with
  | Epsilon(None)            -> "ε"
  | Epsilon(Some(formula))   -> "ε, !" ^ (Ltl.to_string(formula))
  | Sigma([], None)          -> "Σ"
  | Sigma(conds, None)       -> "Σ" ^ (format_conds conds)
  | Sigma([], Some(post))    -> "Σ, !" ^ (Ltl.to_string(post))
  | Sigma(conds, Some(post)) -> "Σ" ^ (format_conds conds) ^ ", !" ^ (Ltl.to_string(post))

let transition_to_string { link = link; s = s; t = t } =
  Printf.sprintf "%s -> %s (%s)" (Ltl.FormulaSet.to_string s) (Ltl.FormulaSet.to_string t) (link_to_string link)

(* known_states: add sigma_transformed states *)
let rec reduction_graph transitions state =
  let is_known trans transitions =
    TransitionSet.exists (OrderedTransition.(=) trans) transitions
  in
  let add_transition trans transitions =
    if is_known trans transitions then
      transitions
    else
      reduction_graph (TransitionSet.add trans transitions) trans.t
  in
  match Ltl.epsilon_transform state with
    | None ->
      let (conds, next) = Ltl.sigma_transform state in
      let trans = { link = Sigma(conds, None); s = state; t = next } in
      add_transition trans transitions
    | Some(conv_list) ->
      List.fold_left (fun transitions (next, cond) ->
        let trans = { link = Epsilon(cond); s = state; t = next } in
        add_transition trans transitions
      ) transitions conv_list

let construct_from start_state =
  { starts = [start_state]; finals = []; transitions = reduction_graph TransitionSet.empty start_state }

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
  ) g (TransitionSet.elements automaton.transitions)
