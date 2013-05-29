type ltl =
  | Top
  | Bottom
  | Prop     of string
  | Not      of ltl
  | And      of ltl * ltl
  | Or       of ltl * ltl
  | Next     of ltl
  | Finally  of ltl
  | Globally of ltl
  | Until    of ltl * ltl
  | Release  of ltl * ltl

module FormulaSet = ExtendedSet.Make
  (struct
    type t = ltl
    let compare = compare
   end)

let rec negative_normal_form formula = match formula with
  | Top | Bottom | Prop(_) -> formula
  | And(l, r)     -> And(negative_normal_form l, negative_normal_form r)
  | Or(l, r)      -> Or(negative_normal_form l, negative_normal_form r)
  | Next(p)       -> Next(negative_normal_form p)
  | Finally(p)    -> Finally(negative_normal_form p)
  | Globally(p)   -> Globally(negative_normal_form p)
  | Until(l, r)   -> Until(negative_normal_form l, negative_normal_form r)
  | Release(l, r) -> Release(negative_normal_form l, negative_normal_form r)
  | Not(formula) ->
    match formula with
      | Top           -> Bottom
      | Bottom        -> Top
      | Prop(_)       -> formula
      | Not(p)        -> negative_normal_form p
      | And(l, r)     -> Or(negative_normal_form (Not l), negative_normal_form (Not r))
      | Or(l, r)      -> And(negative_normal_form (Not l), negative_normal_form (Not r))
      | Next(p)       -> Next(negative_normal_form (Not p))
      | Finally(p)    -> Finally(negative_normal_form (Not p))
      | Globally(p)   -> Globally(negative_normal_form (Not p))
      | Until(l, r)   -> Release(negative_normal_form (Not l), negative_normal_form (Not r))
      | Release(l, r) -> Until(negative_normal_form (Not l), negative_normal_form (Not r))

let is_reduced = function
  | Top          -> true
  | Bottom       -> true
  | Prop(_)      -> true
  | Not(Prop(_)) -> true
  | _            -> false

let epsilon_transform set =
  let apply_rule formula = match formula with
    | And(l, r)     -> [(FormulaSet.of_list [l; r], None)]
    | Or (l, r)     -> [(FormulaSet.singleton l, None);
                        (FormulaSet.singleton r, None)]
    | Release(l, r) -> [(FormulaSet.of_list [l; r], None);
                        (FormulaSet.of_list [r; Next(formula)], None)]
    | Globally(p)   -> [(FormulaSet.of_list [p; Next(p)], None)]
    | Until(l, r)   -> [(FormulaSet.singleton r, None);
                        (FormulaSet.of_list [l; Next(formula)], Some(formula))]
    | Finally(p)    -> [(FormulaSet.singleton p, None);
                        (FormulaSet.singleton(Next(formula)), Some(formula))]
    | _ -> failwith "reduced form given"
  in
  let (reduced, complex) = FormulaSet.partition is_reduced set in
  if FormulaSet.is_empty complex then
    None
  else
    let (formula, complex) = FormulaSet.pop complex in
    let rest = FormulaSet.union reduced complex in
    let transformed = apply_rule formula in
    Some(List.map (fun (set, cond) -> (FormulaSet.union set rest, cond)) transformed)
