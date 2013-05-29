open Ltl
open Graph

let graph_test =
  let g = new_graph "F_p" in
  let g = add_start g "1" in
  let g = add_final g "2" in
  let g = link g "1" "1" "&Sigma;¬p" in
  let g = link g "2" "2" "&Sigma;" in
  let g = link g "1" "2" "&Sigma;p" in
  print_graph g
