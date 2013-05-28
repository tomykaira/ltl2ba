open Syntax

let rec print_ltl exp =
  match exp with
    | Top            -> "⊤"
    | Bottom         -> "⊥"
    | Prop(p)        -> p
    | Not(exp)       -> "¬" ^ (print_ltl exp)
    | And(l, r)      -> (print_ltl l) ^ " ∧ " ^ (print_ltl r)
    | Or(l, r)       -> (print_ltl l) ^ " ∨ " ^ (print_ltl r)
    | Next(exp)      -> "X " ^ (print_ltl exp)
    | Finally(exp)   -> "F " ^ (print_ltl exp)
    | Globally(exp)  -> "G " ^ (print_ltl exp)
    | Until(l, r)    -> (print_ltl l) ^ " U " ^ (print_ltl r)
    | Release(l, r)  -> (print_ltl l) ^ " R " ^ (print_ltl r)
