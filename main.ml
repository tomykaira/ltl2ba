let file num =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 (Printf.sprintf "out_%02d.gv" num)

let rec main num =
  Printf.printf "%02d> " num;
  flush stdout;
  let formula = Ltl.negative_normal_form (Parser.main Lexer.token (Lexing.from_channel stdin)) in
  print_endline (Ltl.to_string formula);
  let formula_set = (Ltl.FormulaSet.singleton formula) in
  let a = Automaton.construct_gba_from formula_set in
  let g = Automaton.to_graph a in
  let out = (file num) in
  Graph.print_graph out g;
  close_out out;
  main (num + 1)

let main_string input =
  let formula = Ltl.negative_normal_form (Parser.main Lexer.token (Lexing.from_string input)) in
  print_endline (Ltl.to_string formula);
  let formula_set = (Ltl.FormulaSet.singleton formula) in
  let a = Automaton.construct_gba_from formula_set in
  let g = Automaton.to_graph a in
  let out = (file 0) in
  Graph.print_graph out g;
  close_out out

let _ = main 0
