{ open Parser } (* Get the token types *)

let num = ['0'-'9']+
let double = (num'.'num | num'.' | '.'num)
let letter = ['a-z' 'A-Z']
let string_lit = '''_*'''

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "//" { single_comment lexbuf }
| "/*" { multi_comment lexbuf } (* Comments *)
| '(' { LPAREN } | ')' { RPAREN } (* Punctuation *)
| '{' { LBRACE } | '}' { RBRACE }
| ';' { SEMI } | ',' { COMMA } | ':' { COLON }
| '+' { PLUS } | '-' { MINUS }
| '*' { TIMES } | '/' { DIVIDE }
| '=' { ASSIGN } | "==" { EQ }
| "&&" { AND } | "||" { OR }
| '!' { NEG } | "!=" { NEQ }
| '<' { LT } | ">" { GT }
| "<=" { LEQ } | ">=" { GEQ }
| "else" { ELSE } | "if" { IF } (* Keywords *)
| "while" { WHILE }
| "int" { INT } | "double" { DOUBLE } | "string" { STRING } | "bool" { BOOL }
| "func" { FUNC } | "return" { RETURN }
| "turn" { TURN } | "move" { MOVE }
| "draw" { DRAW } | "grow" { GROW }
| "print" { PRINT } | "main" { MAIN }
| "gram" { GRAM } | "rules" { RULES } | "init" { INIT }
(* | "->" {ARROW} *)
| "return" { RETURN }
| eof { EOF } (* End-of-file *)
| num as lxm { INT_LIT(int_of_string lxm) } (* integers *)
| double as lxm { DOUBLE_LIT(float_of_string lxm) } (* double *)
| string_lit as lxm { STRING_LIT(lxm) } (* string *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and single_comment = parse
  '\n' { token lexbuf } (* End-of-comment *)
| _    { multi_comment lexbuf } (* Eat everything else *)

and multi_comment = parse
  "*/" { token lexbuf } (* End-of-comment *)
| _    { multi_comment lexbuf } (* Eat everything else *)
