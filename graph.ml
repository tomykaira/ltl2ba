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

let find_node {nodes = nodes} name =
  List.find (fun n -> n.name = name) nodes

let add_node_internal graph node =
  try begin
    ignore (find_node graph node.name);
    graph
  end with
    | Not_found -> { graph with nodes = node :: graph.nodes }

let add_final graph name =
  add_node_internal graph { name = name; shape = DoubleCircle; start = false }

let add_start graph name =
  add_node_internal graph { name = name; shape = Circle; start = true }

let add_start_final graph name =
  add_node_internal graph { name = name; shape = DoubleCircle; start = true }

let add_node graph name =
  add_node_internal graph { name = name; shape = Circle; start = false }

let link graph s_name t_name label =
  { graph with edges = { label = label; s = find_node graph s_name; t = find_node graph t_name } :: graph.edges}

open Printf

let graph_kind_to_string = function
  | DiGraph -> "digraph"
  | Graph   -> "graph"

let print_nodes nodes =
  let print_node_list nodes =
    if not (BatList.is_empty nodes) then
      (List.iter (fun n -> printf "\"%s\" " n.name) nodes;
       printf ";\n")
  in
  let (circle, double) = List.partition (fun n -> n.shape = Circle) nodes in
  printf "\tnode [shape = doublecircle]; ";
  print_node_list double;
  printf "\tnode [shape = circle]; ";
  print_node_list circle;
  let starts = List.find_all (fun n -> n.start) nodes in
  List.iter (fun n -> printf "\t\"_nil_%s\" [style=\"invis\"];\n\t\"_nil_%s\" -> \"%s\";\n" n.name n.name n.name) starts

let print_edges =
  List.iter (fun e ->
    printf "\t\"%s\" -> \"%s\" [ label = \"%s\" ];\n" e.s.name e.t.name e.label
  )

let print_graph { kind = kind; title = title; settings = settings; nodes = nodes; edges = edges } =
  printf "%s %s {\n" (graph_kind_to_string kind) title;
  List.iter (printf "\t%s\n") settings;
  print_nodes nodes;
  print_edges edges;
  printf "}\n"
