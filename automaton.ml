type node = FormulaSet.t
type link = Epsilon of Ltl.ltl option
            | Sigma of Ltl.ltl option * Ltl.ltl option
type edge = {
  link : link;
  s: node; t: node;
}

let reduction_graph root_set =
  let g = ref
