open Ltl

let rec print_ltl exp =
  let print_paren exp =
    "(" ^ (print_ltl exp) ^ ")"
  in
  match exp with
    | Top            -> "⊤"
    | Bottom         -> "⊥"
    | Prop(p)        -> p
    | Not(exp)       -> "¬" ^ (print_paren exp)
    | And(l, r)      -> (print_paren l) ^ " ∧ " ^ (print_paren r)
    | Or(l, r)       -> (print_paren l) ^ " ∨ " ^ (print_paren r)
    | Next(exp)      -> "X " ^ (print_paren exp)
    | Finally(exp)   -> "F " ^ (print_paren exp)
    | Globally(exp)  -> "G " ^ (print_paren exp)
    | Until(l, r)    -> (print_paren l) ^ " U " ^ (print_paren r)
    | Release(l, r)  -> (print_paren l) ^ " R " ^ (print_paren r)
