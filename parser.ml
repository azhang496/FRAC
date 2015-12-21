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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 58 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* COMMA *);
  259 (* COLON *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* LBRACE *);
  263 (* RBRACE *);
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* TIMES *);
  267 (* DIVIDE *);
  268 (* MOD *);
  269 (* ASSIGN *);
  270 (* EQ *);
  271 (* NEQ *);
  272 (* LT *);
  273 (* LEQ *);
  274 (* GT *);
  275 (* GEQ *);
  276 (* OR *);
  277 (* AND *);
  278 (* NOT *);
  279 (* RETURN *);
  280 (* IF *);
  281 (* ELSE *);
  282 (* FOR *);
  283 (* WHILE *);
  284 (* INT *);
  285 (* DOUBLE *);
  286 (* STRING *);
  287 (* BOOL *);
  288 (* GRAM *);
  289 (* ALPHABET *);
  290 (* INIT *);
  291 (* RULES *);
  292 (* LSQUARE *);
  293 (* RSQUARE *);
  294 (* ARROW *);
  295 (* QUOTE *);
  296 (* HYPHEN *);
  297 (* RTURN *);
  298 (* LTURN *);
  299 (* MOVE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  300 (* RULE_ID *);
  301 (* ID *);
  302 (* INT_LIT *);
  303 (* DOUBLE_LIT *);
  304 (* STRING_LIT *);
  305 (* BOOL_LIT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\004\000\004\000\004\000\004\000\005\000\
\005\000\005\000\007\000\007\000\008\000\008\000\009\000\009\000\
\010\000\010\000\010\000\010\000\011\000\011\000\002\000\003\000\
\012\000\012\000\014\000\014\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\013\000\013\000\016\000\016\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\017\000\017\000\018\000\
\018\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\001\000\001\000\001\000\001\000\003\000\
\002\000\005\000\000\000\002\000\001\000\002\000\001\000\003\000\
\008\000\008\000\008\000\007\000\001\000\003\000\022\000\008\000\
\000\000\001\000\001\000\003\000\002\000\003\000\003\000\005\000\
\007\000\009\000\005\000\000\000\002\000\000\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\002\000\003\000\000\000\
\000\000\000\000\004\000\005\000\006\000\007\000\000\000\027\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
\000\000\011\000\028\000\000\000\000\000\000\000\000\000\040\000\
\041\000\043\000\044\000\000\000\000\000\000\000\000\000\046\000\
\000\000\000\000\010\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\000\000\000\015\000\000\000\045\000\000\000\000\000\000\000\
\000\000\000\000\000\000\049\000\050\000\051\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\036\000\024\000\
\000\000\000\000\000\000\000\000\000\000\037\000\000\000\000\000\
\061\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\016\000\000\000\000\000\031\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\035\000\
\000\000\000\000\000\000\013\000\000\000\033\000\000\000\000\000\
\014\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\021\000\000\000\000\000\000\000\000\000\000\000\022\000\023\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\020\000\000\000\000\000\000\000\017\000\018\000\019\000"

let yydgoto = "\002\000\
\003\000\006\000\007\000\015\000\016\000\085\000\037\000\117\000\
\060\000\129\000\130\000\017\000\058\000\018\000\086\000\000\000\
\063\000\064\000"

let yysindex = "\008\000\
\000\000\000\000\225\254\243\254\048\255\000\000\000\000\029\255\
\057\255\060\255\000\000\000\000\000\000\000\000\022\255\000\000\
\092\255\104\255\088\255\033\255\122\255\057\255\107\255\000\000\
\144\255\000\000\000\000\103\255\144\255\144\255\254\254\000\000\
\000\000\000\000\000\000\214\255\057\255\096\255\183\000\000\000\
\144\255\144\255\000\000\144\255\144\255\144\255\144\255\144\255\
\144\255\144\255\144\255\144\255\144\255\144\255\144\255\144\255\
\000\000\077\255\000\000\004\255\000\000\043\001\146\255\134\255\
\043\001\105\255\105\255\000\000\000\000\000\000\069\001\069\001\
\063\255\063\255\063\255\063\255\057\001\057\001\000\000\000\000\
\144\255\138\255\148\255\152\255\235\255\000\000\118\255\172\255\
\000\000\144\255\085\255\001\000\144\255\144\255\144\255\000\000\
\000\000\141\255\043\001\000\000\000\000\200\000\022\000\217\000\
\184\255\123\255\144\255\123\255\149\255\171\255\043\000\000\000\
\153\255\123\255\144\255\000\000\224\254\000\000\234\000\197\255\
\000\000\123\255\165\255\000\000\203\255\210\255\178\255\174\255\
\000\000\006\255\180\255\178\255\213\255\183\255\000\000\000\000\
\008\255\153\255\223\255\233\255\234\255\227\254\144\255\144\255\
\144\255\000\000\251\000\012\001\029\001\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\239\000\000\000\000\000\000\000\000\000\000\000\
\236\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\243\255\000\000\031\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\193\255\000\000\
\000\000\000\000\000\000\000\000\131\255\000\000\000\000\000\000\
\252\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\091\255\000\000\254\255\
\038\255\064\000\085\000\000\000\000\000\000\000\164\000\166\000\
\106\000\114\000\135\000\143\000\043\255\093\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\100\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\137\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\237\255\231\255\000\000\102\000\
\000\000\110\000\000\000\000\000\181\000\000\000\059\000\000\000\
\000\000\000\000"

let yytablesize = 600
let yytable = "\036\000\
\004\000\041\000\027\000\039\000\040\000\087\000\120\000\132\000\
\001\000\146\000\042\000\121\000\133\000\005\000\121\000\062\000\
\065\000\057\000\066\000\067\000\068\000\069\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\008\000\
\009\000\024\000\009\000\009\000\009\000\009\000\060\000\060\000\
\088\000\010\000\060\000\057\000\057\000\025\000\138\000\057\000\
\139\000\140\000\141\000\009\000\009\000\009\000\009\000\092\000\
\009\000\009\000\009\000\009\000\009\000\009\000\057\000\057\000\
\099\000\019\000\020\000\102\000\103\000\104\000\044\000\045\000\
\046\000\047\000\048\000\009\000\009\000\009\000\009\000\009\000\
\029\000\111\000\079\000\080\000\011\000\012\000\013\000\014\000\
\029\000\119\000\079\000\100\000\064\000\058\000\058\000\064\000\
\021\000\058\000\030\000\081\000\082\000\065\000\083\000\084\000\
\065\000\022\000\030\000\081\000\082\000\028\000\083\000\084\000\
\058\000\058\000\046\000\047\000\048\000\147\000\148\000\149\000\
\023\000\031\000\032\000\033\000\034\000\035\000\029\000\026\000\
\079\000\031\000\032\000\033\000\034\000\035\000\036\000\090\000\
\036\000\036\000\038\000\059\000\032\000\093\000\032\000\032\000\
\030\000\081\000\082\000\029\000\083\000\084\000\089\000\094\000\
\036\000\036\000\036\000\095\000\036\000\036\000\032\000\032\000\
\032\000\097\000\032\000\032\000\110\000\030\000\112\000\031\000\
\032\000\033\000\034\000\035\000\118\000\098\000\105\000\036\000\
\036\000\036\000\036\000\036\000\124\000\032\000\032\000\032\000\
\032\000\032\000\109\000\113\000\031\000\032\000\033\000\034\000\
\035\000\042\000\042\000\114\000\116\000\042\000\123\000\125\000\
\042\000\042\000\042\000\042\000\042\000\126\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\043\000\127\000\
\128\000\131\000\134\000\136\000\137\000\044\000\045\000\046\000\
\047\000\048\000\143\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\096\000\144\000\145\000\066\000\142\000\
\025\000\135\000\044\000\045\000\046\000\047\000\048\000\026\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\062\000\101\000\063\000\091\000\000\000\000\000\000\000\000\000\
\044\000\045\000\046\000\047\000\048\000\000\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\107\000\000\000\
\000\000\000\000\000\000\000\000\000\000\044\000\045\000\046\000\
\047\000\048\000\000\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\115\000\000\000\000\000\000\000\000\000\
\000\000\000\000\044\000\045\000\046\000\047\000\048\000\000\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\047\000\047\000\000\000\000\000\047\000\000\000\000\000\047\000\
\047\000\000\000\000\000\000\000\000\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\048\000\048\000\000\000\
\000\000\048\000\000\000\000\000\048\000\048\000\000\000\000\000\
\000\000\000\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\054\000\054\000\000\000\000\000\054\000\000\000\
\000\000\000\000\055\000\055\000\000\000\000\000\055\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\055\000\
\055\000\055\000\055\000\055\000\055\000\055\000\055\000\056\000\
\056\000\000\000\000\000\056\000\000\000\000\000\000\000\059\000\
\059\000\000\000\000\000\059\000\056\000\056\000\056\000\056\000\
\056\000\056\000\056\000\056\000\059\000\059\000\059\000\059\000\
\059\000\059\000\059\000\059\000\052\000\052\000\053\000\053\000\
\052\000\000\000\053\000\000\000\000\000\000\000\000\000\000\000\
\000\000\052\000\052\000\053\000\053\000\000\000\000\000\052\000\
\052\000\053\000\053\000\061\000\000\000\000\000\044\000\045\000\
\046\000\047\000\048\000\000\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\106\000\000\000\000\000\044\000\
\045\000\046\000\047\000\048\000\000\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\108\000\000\000\000\000\
\044\000\045\000\046\000\047\000\048\000\000\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\122\000\000\000\
\000\000\044\000\045\000\046\000\047\000\048\000\000\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\150\000\
\000\000\000\000\044\000\045\000\046\000\047\000\048\000\000\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\151\000\000\000\000\000\044\000\045\000\046\000\047\000\048\000\
\000\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\152\000\000\000\000\000\044\000\045\000\046\000\047\000\
\048\000\000\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\044\000\045\000\046\000\047\000\048\000\000\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\044\000\045\000\046\000\047\000\048\000\000\000\049\000\050\000\
\051\000\052\000\053\000\054\000\044\000\045\000\046\000\047\000\
\048\000\000\000\000\000\000\000\051\000\052\000\053\000\054\000"

let yycheck = "\025\000\
\032\001\004\001\022\000\029\000\030\000\002\001\039\001\002\001\
\001\000\039\001\013\001\044\001\007\001\045\001\044\001\041\000\
\042\000\037\000\044\000\045\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\045\001\
\002\001\001\001\004\001\005\001\006\001\007\001\001\001\002\001\
\037\001\013\001\005\001\001\001\002\001\013\001\039\001\005\001\
\041\001\042\001\043\001\004\001\022\001\023\001\024\001\081\000\
\026\001\027\001\028\001\029\001\030\001\031\001\020\001\021\001\
\090\000\006\001\045\001\093\000\094\000\095\000\008\001\009\001\
\010\001\011\001\012\001\045\001\046\001\047\001\048\001\049\001\
\004\001\107\000\006\001\007\001\028\001\029\001\030\001\031\001\
\004\001\115\000\006\001\007\001\002\001\001\001\002\001\005\001\
\005\001\005\001\022\001\023\001\024\001\002\001\026\001\027\001\
\005\001\002\001\022\001\023\001\024\001\003\001\026\001\027\001\
\020\001\021\001\010\001\011\001\012\001\143\000\144\000\145\000\
\033\001\045\001\046\001\047\001\048\001\049\001\004\001\006\001\
\006\001\045\001\046\001\047\001\048\001\049\001\004\001\002\001\
\006\001\007\001\036\001\044\001\004\001\004\001\006\001\007\001\
\022\001\023\001\024\001\004\001\026\001\027\001\005\001\004\001\
\022\001\023\001\024\001\004\001\026\001\027\001\022\001\023\001\
\024\001\044\001\026\001\027\001\106\000\022\001\108\000\045\001\
\046\001\047\001\048\001\049\001\114\000\002\001\034\001\045\001\
\046\001\047\001\048\001\049\001\122\000\045\001\046\001\047\001\
\048\001\049\001\003\001\039\001\045\001\046\001\047\001\048\001\
\049\001\001\001\002\001\025\001\044\001\005\001\002\001\035\001\
\008\001\009\001\010\001\011\001\012\001\003\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\001\001\006\001\
\039\001\044\001\039\001\007\001\038\001\008\001\009\001\010\001\
\011\001\012\001\004\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\001\001\004\001\004\001\000\000\138\000\
\005\001\132\000\008\001\009\001\010\001\011\001\012\001\005\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\005\001\001\001\005\001\079\000\255\255\255\255\255\255\255\255\
\008\001\009\001\010\001\011\001\012\001\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\001\001\255\255\
\255\255\255\255\255\255\255\255\255\255\008\001\009\001\010\001\
\011\001\012\001\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\001\001\255\255\255\255\255\255\255\255\
\255\255\255\255\008\001\009\001\010\001\011\001\012\001\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\001\001\002\001\255\255\255\255\005\001\255\255\255\255\008\001\
\009\001\255\255\255\255\255\255\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\001\001\002\001\255\255\
\255\255\005\001\255\255\255\255\008\001\009\001\255\255\255\255\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\001\001\002\001\255\255\255\255\005\001\255\255\
\255\255\255\255\001\001\002\001\255\255\255\255\005\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\001\001\
\002\001\255\255\255\255\005\001\255\255\255\255\255\255\001\001\
\002\001\255\255\255\255\005\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\001\001\002\001\001\001\002\001\
\005\001\255\255\005\001\255\255\255\255\255\255\255\255\255\255\
\255\255\014\001\015\001\014\001\015\001\255\255\255\255\020\001\
\021\001\020\001\021\001\005\001\255\255\255\255\008\001\009\001\
\010\001\011\001\012\001\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\005\001\255\255\255\255\008\001\
\009\001\010\001\011\001\012\001\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\005\001\255\255\255\255\
\008\001\009\001\010\001\011\001\012\001\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\005\001\255\255\
\255\255\008\001\009\001\010\001\011\001\012\001\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\005\001\
\255\255\255\255\008\001\009\001\010\001\011\001\012\001\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\005\001\255\255\255\255\008\001\009\001\010\001\011\001\012\001\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\005\001\255\255\255\255\008\001\009\001\010\001\011\001\
\012\001\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\008\001\009\001\010\001\011\001\012\001\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\008\001\009\001\010\001\011\001\012\001\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\008\001\009\001\010\001\011\001\
\012\001\255\255\255\255\255\255\016\001\017\001\018\001\019\001"

let yynames_const = "\
  SEMI\000\
  COMMA\000\
  COLON\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MOD\000\
  ASSIGN\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  OR\000\
  AND\000\
  NOT\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  DOUBLE\000\
  STRING\000\
  BOOL\000\
  GRAM\000\
  ALPHABET\000\
  INIT\000\
  RULES\000\
  LSQUARE\000\
  RSQUARE\000\
  ARROW\000\
  QUOTE\000\
  HYPHEN\000\
  RTURN\000\
  LTURN\000\
  MOVE\000\
  EOF\000\
  "

let yynames_block = "\
  RULE_ID\000\
  ID\000\
  INT_LIT\000\
  DOUBLE_LIT\000\
  STRING_LIT\000\
  BOOL_LIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                       ( [], [] )
# 427 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'gdecl) in
    Obj.repr(
# 38 "parser.mly"
                       ( let (grams, funcs) = _1 in _2::grams, funcs )
# 435 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 39 "parser.mly"
                       ( let (grams, funcs) = _1 in grams, _2::funcs )
# 443 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
        ( Int )
# 449 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
           ( Double )
# 455 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
           ( String )
# 461 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
         ( Bool )
# 467 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 50 "parser.mly"
                                  ( Var(_1, _2))
# 475 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "parser.mly"
                                  ( Var(_1, _2))
# 483 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                                  ( Var_Init(_1, _2, _4))
# 492 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                     ( [] )
# 498 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 56 "parser.mly"
                     ( _2 :: _1 )
# 506 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
                                ( [_1] )
# 513 "parser.ml"
               : 'rule_id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rule_id_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                          ( _2 :: _1 )
# 521 "parser.ml"
               : 'rule_id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                                ( [_1] )
# 528 "parser.ml"
               : 'comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comma_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                              ( _3 :: _1 )
# 536 "parser.ml"
               : 'comma_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                                                         ( Term(_2, Rturn(_7)) )
# 544 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                                                         ( Term(_2, Lturn(_7)) )
# 552 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                                                         ( Term(_2, Move(_7)) )
# 560 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'rule_id_list) in
    Obj.repr(
# 72 "parser.mly"
                                                         ( Rec(_2, List.rev _6) )
# 568 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 75 "parser.mly"
                          ( [_1] )
# 575 "parser.ml"
               : 'rule_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rule_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 76 "parser.mly"
                          ( _3 :: _1 )
# 583 "parser.ml"
               : 'rule_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 20 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 14 : 'comma_list) in
    let _14 = (Parsing.peek_val __caml_parser_env 8 : 'rule_id_list) in
    let _20 = (Parsing.peek_val __caml_parser_env 2 : 'rule_list) in
    Obj.repr(
# 86 "parser.mly"
    ( { gname = _2;
        alphabet = _8;
        init = _14;
        rules = List.rev _20 } )
# 596 "parser.ml"
               : 'gdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 95 "parser.mly"
     ( { fname = _1;
     formals = _3;
     locals = List.rev _6;
     body = List.rev _7 } )
# 609 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
                  ( [] )
# 615 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 102 "parser.mly"
                  ( List.rev _1 )
# 622 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 105 "parser.mly"
                           ( [_1] )
# 629 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 106 "parser.mly"
                            ( _3 :: _1 )
# 637 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                                                                  ( Expr(_1) )
# 644 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                                                                  ( Return(_2) )
# 651 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 113 "parser.mly"
                                                                  ( Block(List.rev _2) )
# 658 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 114 "parser.mly"
                                                                  ( If(_3, _5, Block([])) )
# 666 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 115 "parser.mly"
                                                                  ( If(_3, _5, _7) )
# 675 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 116 "parser.mly"
                                                                 ( For(_3, _5, _7, _9) )
# 685 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 117 "parser.mly"
                                                                  ( While(_3, _5) )
# 693 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
                   ( [] )
# 699 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 121 "parser.mly"
                   ( _2 :: _1 )
# 707 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
                  ( Noexpr )
# 713 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                  ( _1 )
# 720 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 130 "parser.mly"
                     ( Int_lit(_1) )
# 727 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 131 "parser.mly"
                     ( Double_lit(_1) )
# 734 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 132 "parser.mly"
                     ( Id(_1) )
# 741 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 133 "parser.mly"
                     ( String_lit(_1) )
# 748 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 134 "parser.mly"
                     ( Bool_lit(_1) )
# 755 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                       ( ParenExpr(_2) )
# 762 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                  ( Unop(Not, _2) )
# 769 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 777 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 785 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 793 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 801 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                     ( Binop(_1, Mod,   _3) )
# 809 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 817 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 825 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 833 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 841 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 849 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
                     ( Binop(_1, Or,    _3) )
# 857 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                     ( Binop(_1, And,   _3) )
# 865 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 873 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 150 "parser.mly"
                     ( Assign(_1, _3) )
# 881 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 151 "parser.mly"
                                 ( Call(_1, _3) )
# 889 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 155 "parser.mly"
                  ( [] )
# 895 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 156 "parser.mly"
                  ( List.rev _1 )
# 902 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "parser.mly"
                            ( [_1] )
# 909 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 160 "parser.mly"
                            ( _3 :: _1 )
# 917 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
