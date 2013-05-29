type shape = DoubleCircle | Circle
type graph_kind = DiGraph | Graph

type node = {
  name : string;
  shape : shape;
}

type edge = {
  label : string;
  s     : node;
  t     : node;
}

type graph = {
  kind     : graph_kind;
  title    : string;
  settings : string list;
  nodes    : node list;
  edges    : edge list;
}

open Printf

let graph_kind_to_string = function
  | DiGraph -> "digraph"
  | Graph   -> "graph"

let print_nodes nodes =
  let (circle, double) = List.partition (fun n -> n.shape = Circle) nodes in
  printf "\tnode [shape = doublecircle]; ";
  List.iter (fun n -> printf "%s " n.name) double;
  printf ";\n";
  printf "\tnode [shape = circle]; ";
  List.iter (fun n -> printf "%s " n.name) circle;
  printf ";\n"

let print_edges =
  List.iter (fun e ->
    printf "\t%s -> %s [ label = \"%s\" ];\n" e.s.name e.t.name e.label
  )

let print_graph { kind = kind; title = title; settings = settings; nodes = nodes; edges = edges } =
  printf "%s %s {\n" (graph_kind_to_string kind) title;
  List.iter (printf "\t%s\n") settings;
  print_nodes nodes;
  print_edges edges;
  printf "}\n"
