open AST

type token =
  | INT of (int)
  | VARIABLE of (string)
  | PLUS
  | MINUS
  | TIMES
  | LOG
  | POW
  | OPENPAREN
  | CLOSEPAREN
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> algebraic
