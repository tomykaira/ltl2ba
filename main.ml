let graph_test =
  let formula = Parser.main Lexer.token (Lexing.from_string "G((not p) or (F q))\n") in
  let a = Automaton.reduction_graph (Ltl.FormulaSet.singleton formula) in
  let g = Automaton.to_graph a in
  Graph.print_graph g
