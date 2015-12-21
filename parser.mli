type token =
  | SEMI
  | COMMA
  | COLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | OR
  | AND
  | NOT
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | DOUBLE
  | STRING
  | BOOL
  | GRAM
  | ALPHABET
  | INIT
  | RULES
  | LSQUARE
  | RSQUARE
  | ARROW
  | QUOTE
  | HYPHEN
  | RTURN
  | LTURN
  | MOVE
  | RULE_ID of (string)
  | ID of (string)
  | INT_LIT of (int)
  | DOUBLE_LIT of (float)
  | STRING_LIT of (string)
  | BOOL_LIT of (bool)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
