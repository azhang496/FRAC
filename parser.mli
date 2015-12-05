type token =
  | SEMI
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | ARROW
  | RETURN
  | IF
  | ELSE
  | WHILE
  | INT
  | DOUBLE
  | STRING
  | BOOL
  | INT_LIT of (int)
  | DOUBLE_LIT of (float)
  | ID of (string)
  | STRING_LIT of (string)
  | BOOL_LIT of (bool)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
