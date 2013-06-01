type link = Epsilon of Ltl.ltl list
            | Sigma of Ltl.ltl list * Ltl.ltl list
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
  | Epsilon(postpones)       -> "ε" ^ (BatString.join "" (List.map (fun f -> ", !" ^ (Ltl.to_string(f))) postpones))
  | Sigma(conds, postpones)  -> "Σ" ^ (format_conds conds) ^ (BatString.join "" (List.map (fun f -> ", " ^ (Ltl.to_string(f))) postpones))

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
  let epsilon_from_option = function
    | None    -> Epsilon([])
    | Some(p) -> Epsilon([p])
  in
  match Ltl.epsilon_transform state with
    | None ->
      let (conds, next) = Ltl.sigma_transform state in
      let trans = { link = Sigma(conds, []); s = state; t = next } in
      add_transition trans transitions
    | Some(conv_list) ->
      List.fold_left (fun transitions (next, cond) ->
        let trans = { link = epsilon_from_option cond; s = state; t = next } in
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

let name_counter = ref 0
let snapshot automaton transition =
  let file () =
    name_counter := !name_counter + 1;
    open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 (Printf.sprintf "t_%02d.gv" !name_counter)
  in
  Graph.print_graph (file ()) (to_graph { automaton with transitions = TransitionSet.of_list transition })

let unique_postpones transitions =
  BatList.unique (List.fold_left (fun postponed { link = link } ->
    match link with
      | Epsilon(p) -> p @ postponed
      | _ -> postponed
  ) [] transitions)

let setup_sigma_postpones postpones  =
  List.map (fun trans ->
    match trans.link with
      | Sigma(c, p) -> { trans with link = Sigma(c, p @ postpones) }
      | _ -> trans)

let skip_epsilons automaton =
  let transitions = TransitionSet.elements automaton.transitions in
  let postpones   = unique_postpones transitions in
  let transitions = setup_sigma_postpones postpones transitions in
  let rec skip transitions =
    snapshot automaton transitions;
    let (epsilons, sigmas) = List.partition (fun t ->
      match t.link with
        | Epsilon(_) -> true | Sigma(_, _) -> false
    ) transitions in
    if BatList.is_empty epsilons then
      sigmas
    else
      let rest = List.tl epsilons @ sigmas in
      let target = List.hd epsilons in
      if Ltl.FormulaSet.(target.s = target.t) then
        skip rest
      else
        let replace rewrite_rule =
          let (nexts, rest) = List.partition (fun t -> Ltl.FormulaSet.(t.s = target.t)) rest in
          let transitions = List.fold_left (fun transitions n ->
            if List.exists (fun t -> Ltl.FormulaSet.(t.t = n.s)) transitions then (* still referred *)
              (rewrite_rule n) :: n :: transitions
            else
              (rewrite_rule n) :: transitions
          ) rest nexts in
          skip transitions
        in
        match target.link with
          | Epsilon([]) ->
            replace (fun next -> { next with s = target.s })
          | Epsilon(ps) ->
            let new_link = function
              | Epsilon(ps')  -> Epsilon(ps @ ps')
              | Sigma(c, constraints) -> Sigma(c, List.find_all (fun p -> not (List.exists ((=) p) ps)) constraints)
            in
            replace (fun next -> { next with s = target.s; link = new_link next.link })
          | _ -> failwith "unexpected non-epsilon value"
  in
  { automaton with transitions = TransitionSet.of_list (skip transitions) }
