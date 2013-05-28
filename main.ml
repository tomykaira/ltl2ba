open Syntax

let main =
  print_string
    (Printer.print_ltl
       (Parser.main Lexer.token
          (Lexing.from_channel stdin)))
