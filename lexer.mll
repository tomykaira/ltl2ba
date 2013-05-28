{
  open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''

let prop = ['a'-'z']

rule token = parse
  | prop     { PROP (Lexing.lexeme lexbuf) }
  | "top"    { TOP }
  | "bottom" { BOTTOM }
  | "!"      { NOT }
  | "not"    { NOT }
  | "&&"     { AND }
  | "and"    { AND }
  | "||"     { OR }
  | "or"     { OR }
  | "X"      { NEXT }
  | "N"      { NEXT }
  | "F"      { FINALLY }
  | "G"      { GLOBALLY }
  | "U"      { UNTIL }
  | "R"      { RELEASE }

  | '\n'     { EOL }
