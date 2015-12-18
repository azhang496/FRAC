(*
todo:
  + check single line comments working
  + comment/organize this better it's ugly
*)

{ open Parser }

let num = ('-')?['0'-'9']+
let dbl = ('-')?(['0'-'9']+'.'['0'-'9']+ | '.'num)
let boolean = "true" | "false"

rule token = parse
(* Whitespace *)
  [' ' '\t' '\r' '\n'] { token lexbuf }
(* Comments *)
| "/*"     { multi_comment lexbuf }
| "//"     { single_comment lexbuf }
(* Punctuation *)
| '('      { LPAREN } | ')'      { RPAREN }
| '{'      { LBRACE } | '}'      { RBRACE }
| ';'      { SEMI }   | ','      { COMMA }
(* Arithmetic Operators *)
| '+'      { PLUS }   | '-'      { MINUS }
| '*'      { TIMES }  | '/'      { DIVIDE }
| '%'      { MOD }    | '='      { ASSIGN }
(* Logical Operators *)
| "=="     { EQ }     | "!="     { NEQ }
| '<'      { LT }     | "<="     { LEQ }
| ">"      { GT }     | ">="     { GEQ }
| "||"     { OR }     | "&&"     { AND }
| '!'      { NOT }

(* Grammar Syntax *)
| "gram"   { GRAM }  | "rules"    { RULES }
| "init"   { INIT }  | "alphabet" { ALPHABET }
| ":"      { COLON } | "'"        { QUOTE }
| "->"     { ARROW } 
| ['a'-'z' 'A'-'Z'] as lxm { RULE_ID (lxm) }

(* Statements *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }

(* Type Names *)
| "int"    { INT }
| "double" { DOUBLE }
| "string" { STRING }
| "bool"   { BOOL }
| '"'      { read_string (Buffer.create 17) lexbuf }
| num as lxm { INT_LIT (int_of_string lxm) }
| dbl as lxm { DOUBLE_LIT (float_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID (lxm) }
| boolean as lxm  { BOOL_LIT (bool_of_string lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and read_string buf =
  parse
  | '"'       { STRING_LIT (Buffer.contents buf) } (*
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Failure ("Illegal string character: " ^ Lexing.lexeme lexbuf)) } *)
  | _ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf}
  | eof { raise (Failure ("String is not terminated")) }

and multi_comment = parse
  "*/" { token lexbuf }
| _    { multi_comment lexbuf }

and single_comment = parse
  '\n' { token lexbuf }
| _    { single_comment lexbuf }
