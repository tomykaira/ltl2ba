open Syntax
open Graph

let graph_test =
  let lr0 = { name = "LR_0"; shape = DoubleCircle } in
  let lr1 = { name = "LR_1"; shape = Circle } in
  print_graph {
    kind = DiGraph;
    title = "FSM";
    settings = ["size=\"8,5\""];
    nodes = [ lr0; lr1 ];
    edges = [ { label = "S(b)"; s = lr0; t = lr1 }]
  }
