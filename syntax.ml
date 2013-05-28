type exp =
  | Top
  | Bottom
  | Prop     of string
  | Not      of exp
  | And      of exp * exp
  | Or       of exp * exp
  | Next     of exp
  | Finally  of exp
  | Globally of exp
  | Until    of exp * exp
  | Relase   of exp * exp
