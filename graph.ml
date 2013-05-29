type shape = DoubleCircle | Circle
type graph_kind = DiGraph | Graph

type node = {
  name  : string;
  shape : shape;
  start : bool;
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

let new_graph title =
  { kind = DiGraph; title = title; settings = []; nodes = []; edges = [] }

let add_final graph name =
  { graph with nodes = { name = name; shape = DoubleCircle; start = false } :: graph.nodes }

let add_start graph name =
  { graph with nodes = { name = name; shape = Circle; start = true } :: graph.nodes }

let add_start_final graph name =
  { graph with nodes = { name = name; shape = DoubleCircle; start = true } :: graph.nodes }

let add_node graph name =
  { graph with nodes = { name = name; shape = Circle; start = false } :: graph.nodes }

let find_node {nodes = nodes} name =
  List.find (fun n -> n.name = name) nodes

let link graph s_name t_name label =
  { graph with edges = { label = label; s = find_node graph s_name; t = find_node graph t_name } :: graph.edges}

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
  printf ";\n";
  let starts = List.find_all (fun n -> n.start) nodes in
  List.iter (fun n -> printf "\t_nil_%s [style=\"invis\"];\n\t_nil_%s -> %s;\n" n.name n.name n.name) starts

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
