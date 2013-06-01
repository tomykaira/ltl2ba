let graph_test =
  let formula = Parser.main Lexer.token (Lexing.from_string "G((not p) or (F q))\n") in
  let formula_set = (Ltl.FormulaSet.singleton formula) in
  let a = Automaton.construct_gba_from formula_set in
  let g = Automaton.to_graph a in
  Graph.print_graph stdout g
