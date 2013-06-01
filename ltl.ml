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

let rec to_string exp =
  let print_paren exp =
    match exp with
      | Top | Bottom | Prop(_) | Not(_) | Next(_) | Finally(_) | Globally(_)
          -> to_string exp
      | _ -> "(" ^ (to_string exp) ^ ")"

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

let rec size_of exp =
  match exp with
    | Top
    | Bottom
    | Prop(_) -> 1
    | Not(exp)
    | Next(exp)
    | Finally(exp)
    | Globally(exp)  -> 1 + size_of exp
    | And(l, r)
    | Or(l, r)
    | Until(l, r)
    | Release(l, r)  -> 1 + max (size_of l) (size_of r)

module FormulaSet =
  struct
    module S_ = ExtendedSet.Make
      (struct
        type t = ltl
        let compare = compare
       end)
    include S_

    (* avoid bug *)
    let compare a b = Pervasives.compare (elements a) (elements b)
    let (=) a b     = compare a b = 0

    let to_string set =
      let string_formulae = List.map to_string (elements set) in
      "{ " ^ (BatString.join ", " string_formulae) ^ "}"

    let pop_largest set =
      let (_, largest) = List.fold_left (fun (size, value) formula ->
        let this_size = size_of formula in
        if size < this_size then
          (this_size, formula)
        else
          (size, value)
      ) (0, Bottom) (elements set) in
      (largest, remove largest set)
  end




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
  | Next(_)      -> true
  | _            -> false

let epsilon_transform set =
  let apply_rule formula = match formula with
    | And(l, r)     -> [(FormulaSet.of_list [l; r], None)]
    | Or (l, r)     -> [(FormulaSet.singleton l, None);
                        (FormulaSet.singleton r, None)]
    | Release(l, r) -> [(FormulaSet.of_list [l; r], None);
                        (FormulaSet.of_list [r; Next(formula)], None)]
    | Globally(p)   -> [(FormulaSet.of_list [p; Next(formula)], None)]
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
    let (formula, complex) = FormulaSet.pop_largest complex in
    let rest = FormulaSet.union reduced complex in
    let transformed = apply_rule formula in
    Some(List.map (fun (set, cond) -> (FormulaSet.union set rest, cond)) transformed)

(*
  calculate sigma transform condition and result set
  input set should be reduced and consistent
*)
let sigma_transform set =
  List.fold_left (fun (conds, next) -> function
    | Top    -> (conds, next)
    | Bottom -> failwith "inconsistent Bottom"
    | Prop(p) ->
      if List.exists (fun c -> c = Not(Prop(p))) conds then
        failwith ("inconsistent " ^ p ^ " and not " ^ p)
      else
        (Prop(p) :: conds, next)
    | Not(Prop(p)) ->
      if List.exists (fun c -> c = Prop(p)) conds then
        failwith ("inconsistent " ^ p ^ " and not " ^ p)
      else
        (Not(Prop(p)) :: conds, next)
    | Next(x) ->
      (conds, FormulaSet.add x next)
    | other -> failwith ("not reduced " ^ to_string other)
  ) ([], FormulaSet.empty) (FormulaSet.elements set)



(* prop is simple NNF. prop1 < prop2 *)
let calculate_or prop1 prop2 =
  let rec merge prop1 prop2 =
    if prop1 = prop2 then
      Some(prop1)
    else if size_of prop1 > size_of prop2 then
      merge prop2 prop1
    else
      match prop1 with
        | Top    -> Some(Top)
        | Bottom -> Some(prop2)
        | _ ->
          match prop2 with
            | Top -> Some(Top)
            | Bottom -> Some(prop1)
            | Prop(_) -> None
            | Not(_)  -> None
            | And(l, r) -> begin
              match merge prop1 l with
                | Some(result) -> Some(result)
                | None -> merge prop1 r
            end
            | Or(l, r) -> begin
              match merge prop1 l with
                | Some(result) -> Some(Or(result, r))
                | None ->
                  match merge prop1 r with
                    | Some(result) -> Some(Or(l, result))
                    | None -> None
            end
            | _ -> failwith "Not in propositional language"
  in
  match merge prop1 prop2 with
    | Some(result) -> result
    | None -> Or(prop1, prop2)

let and_concat props =
  match props with
    | [] -> Top
    | _ -> BatList.reduce (fun a b -> And(a, b)) props
