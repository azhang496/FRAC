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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 40 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* COMMA *);
  259 (* LPAREN *);
  260 (* RPAREN *);
  261 (* LBRACE *);
  262 (* RBRACE *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* ASSIGN *);
  268 (* EQ *);
  269 (* NEQ *);
  270 (* LT *);
  271 (* LEQ *);
  272 (* GT *);
  273 (* GEQ *);
  274 (* ARROW *);
  275 (* RETURN *);
  276 (* IF *);
  277 (* ELSE *);
  278 (* WHILE *);
  279 (* INT *);
  280 (* DOUBLE *);
  281 (* STRING *);
  282 (* BOOL *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  283 (* INT_LIT *);
  284 (* DOUBLE_LIT *);
  285 (* ID *);
  286 (* STRING_LIT *);
  287 (* BOOL_LIT *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\003\000\004\000\004\000\006\000\
\006\000\007\000\002\000\002\000\008\000\008\000\010\000\010\000\
\011\000\011\000\011\000\011\000\011\000\011\000\009\000\009\000\
\012\000\012\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\013\000\013\000\014\000\014\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\003\000\005\000\000\000\
\002\000\008\000\000\000\002\000\000\000\001\000\001\000\003\000\
\002\000\003\000\003\000\005\000\007\000\005\000\000\000\002\000\
\000\000\001\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\003\000\000\000\001\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\011\000\000\000\049\000\000\000\000\000\001\000\012\000\000\000\
\015\000\000\000\000\000\000\000\000\000\008\000\016\000\000\000\
\002\000\003\000\004\000\005\000\000\000\009\000\000\000\000\000\
\000\000\023\000\010\000\000\000\000\000\000\000\027\000\028\000\
\000\000\030\000\031\000\000\000\024\000\006\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\044\000\019\000\018\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\035\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\043\000\000\000\000\000\022\000\000\000\000\000\021\000"

let yydgoto = "\002\000\
\003\000\004\000\021\000\022\000\036\000\016\000\007\000\010\000\
\023\000\011\000\037\000\000\000\065\000\066\000"

let yysindex = "\003\000\
\000\000\000\000\000\000\001\000\019\255\000\000\000\000\006\255\
\000\000\029\255\042\255\051\255\036\255\000\000\000\000\058\255\
\000\000\000\000\000\000\000\000\038\255\000\000\031\255\001\255\
\012\255\000\000\000\000\012\255\082\255\097\255\000\000\000\000\
\035\255\000\000\000\000\146\255\000\000\000\000\012\255\019\000\
\049\255\163\255\012\255\012\255\012\255\012\255\000\000\012\255\
\012\255\012\255\012\255\012\255\012\255\012\255\012\255\012\255\
\012\255\180\255\000\000\000\000\000\000\033\000\047\000\058\000\
\102\255\100\255\058\000\039\255\039\255\000\000\000\000\069\000\
\069\000\101\255\101\255\101\255\101\255\000\000\098\255\098\255\
\000\000\012\255\103\255\000\000\058\000\098\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\115\255\
\000\000\000\000\117\255\000\000\000\000\000\000\000\000\067\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\129\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\118\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\255\
\000\000\119\255\009\255\197\255\214\255\000\000\000\000\004\255\
\062\255\232\255\238\255\255\255\005\000\000\000\000\000\000\000\
\000\000\000\000\085\255\000\000\043\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\231\255\000\000\000\000\000\000\
\106\000\000\000\013\000\000\000\000\000\000\000"

let yytablesize = 342
let yytable = "\040\000\
\006\000\038\000\042\000\001\000\036\000\036\000\047\000\036\000\
\047\000\042\000\042\000\039\000\042\000\058\000\025\000\036\000\
\036\000\062\000\063\000\064\000\067\000\008\000\068\000\069\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\012\000\025\000\009\000\026\000\027\000\045\000\031\000\032\000\
\033\000\034\000\035\000\013\000\048\000\046\000\048\000\050\000\
\051\000\028\000\029\000\025\000\030\000\026\000\060\000\014\000\
\085\000\031\000\032\000\033\000\034\000\035\000\037\000\037\000\
\015\000\037\000\024\000\028\000\029\000\023\000\030\000\023\000\
\023\000\037\000\037\000\031\000\032\000\033\000\034\000\035\000\
\017\000\018\000\019\000\020\000\043\000\023\000\023\000\020\000\
\023\000\020\000\020\000\083\000\084\000\023\000\023\000\023\000\
\023\000\023\000\087\000\044\000\025\000\082\000\026\000\020\000\
\020\000\081\000\020\000\048\000\049\000\050\000\051\000\020\000\
\020\000\020\000\020\000\020\000\028\000\029\000\013\000\030\000\
\014\000\045\000\046\000\086\000\031\000\032\000\033\000\034\000\
\035\000\029\000\029\000\041\000\029\000\000\000\000\000\029\000\
\029\000\029\000\029\000\000\000\029\000\029\000\029\000\029\000\
\029\000\029\000\047\000\000\000\000\000\000\000\000\000\000\000\
\048\000\049\000\050\000\051\000\000\000\052\000\053\000\054\000\
\055\000\056\000\057\000\061\000\000\000\000\000\000\000\000\000\
\000\000\048\000\049\000\050\000\051\000\000\000\052\000\053\000\
\054\000\055\000\056\000\057\000\078\000\000\000\000\000\000\000\
\000\000\000\000\048\000\049\000\050\000\051\000\000\000\052\000\
\053\000\054\000\055\000\056\000\057\000\032\000\032\000\000\000\
\032\000\000\000\000\000\032\000\032\000\000\000\000\000\000\000\
\032\000\032\000\032\000\032\000\032\000\032\000\033\000\033\000\
\000\000\033\000\000\000\000\000\033\000\033\000\000\000\000\000\
\000\000\033\000\033\000\033\000\033\000\033\000\033\000\000\000\
\038\000\038\000\000\000\038\000\000\000\000\000\039\000\039\000\
\000\000\039\000\000\000\038\000\038\000\038\000\038\000\038\000\
\038\000\039\000\039\000\039\000\039\000\039\000\039\000\040\000\
\040\000\000\000\040\000\000\000\000\000\041\000\041\000\000\000\
\041\000\000\000\040\000\040\000\040\000\040\000\040\000\040\000\
\041\000\041\000\041\000\041\000\041\000\041\000\059\000\000\000\
\000\000\048\000\049\000\050\000\051\000\005\000\052\000\053\000\
\054\000\055\000\056\000\057\000\079\000\000\000\000\000\048\000\
\049\000\050\000\051\000\000\000\052\000\053\000\054\000\055\000\
\056\000\057\000\080\000\000\000\000\000\048\000\049\000\050\000\
\051\000\000\000\052\000\053\000\054\000\055\000\056\000\057\000\
\048\000\049\000\050\000\051\000\000\000\052\000\053\000\054\000\
\055\000\056\000\057\000\048\000\049\000\050\000\051\000\000\000\
\000\000\000\000\054\000\055\000\056\000\057\000"

let yycheck = "\025\000\
\000\000\001\001\028\000\001\000\001\001\002\001\002\001\004\001\
\004\001\001\001\002\001\011\001\004\001\039\000\003\001\012\001\
\013\001\043\000\044\000\045\000\046\000\003\001\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\004\001\003\001\029\001\005\001\006\001\003\001\027\001\028\001\
\029\001\030\001\031\001\002\001\002\001\011\001\004\001\009\001\
\010\001\019\001\020\001\003\001\022\001\005\001\006\001\005\001\
\082\000\027\001\028\001\029\001\030\001\031\001\001\001\002\001\
\029\001\004\001\029\001\019\001\020\001\003\001\022\001\005\001\
\006\001\012\001\013\001\027\001\028\001\029\001\030\001\031\001\
\023\001\024\001\025\001\026\001\003\001\019\001\020\001\003\001\
\022\001\005\001\006\001\079\000\080\000\027\001\028\001\029\001\
\030\001\031\001\086\000\003\001\003\001\002\001\005\001\019\001\
\020\001\004\001\022\001\007\001\008\001\009\001\010\001\027\001\
\028\001\029\001\030\001\031\001\019\001\020\001\004\001\022\001\
\004\001\004\001\004\001\021\001\027\001\028\001\029\001\030\001\
\031\001\001\001\002\001\026\000\004\001\255\255\255\255\007\001\
\008\001\009\001\010\001\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\001\001\255\255\255\255\255\255\255\255\255\255\
\007\001\008\001\009\001\010\001\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\001\001\255\255\255\255\255\255\255\255\
\255\255\007\001\008\001\009\001\010\001\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\001\001\255\255\255\255\255\255\
\255\255\255\255\007\001\008\001\009\001\010\001\255\255\012\001\
\013\001\014\001\015\001\016\001\017\001\001\001\002\001\255\255\
\004\001\255\255\255\255\007\001\008\001\255\255\255\255\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\001\001\002\001\
\255\255\004\001\255\255\255\255\007\001\008\001\255\255\255\255\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\255\255\
\001\001\002\001\255\255\004\001\255\255\255\255\001\001\002\001\
\255\255\004\001\255\255\012\001\013\001\014\001\015\001\016\001\
\017\001\012\001\013\001\014\001\015\001\016\001\017\001\001\001\
\002\001\255\255\004\001\255\255\255\255\001\001\002\001\255\255\
\004\001\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\012\001\013\001\014\001\015\001\016\001\017\001\004\001\255\255\
\255\255\007\001\008\001\009\001\010\001\029\001\012\001\013\001\
\014\001\015\001\016\001\017\001\004\001\255\255\255\255\007\001\
\008\001\009\001\010\001\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\004\001\255\255\255\255\007\001\008\001\009\001\
\010\001\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\007\001\008\001\009\001\010\001\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\014\001\015\001\016\001\017\001"

let yynames_const = "\
  SEMI\000\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  ARROW\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  INT\000\
  DOUBLE\000\
  STRING\000\
  BOOL\000\
  EOF\000\
  "

let yynames_block = "\
  INT_LIT\000\
  DOUBLE_LIT\000\
  ID\000\
  STRING_LIT\000\
  BOOL_LIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl_list) in
    Obj.repr(
# 31 "parser.mly"
                 ( _1 )
# 280 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
        ( Int )
# 286 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
           ( Double )
# 292 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parser.mly"
           ( String )
# 298 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
         ( Bool )
# 304 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 49 "parser.mly"
      ( { vtype = _1;
      vname = _2;
      value = Noexpr } )
# 314 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
      ( { vtype = _1;
      vname = _2;
      value = _4 } )
# 325 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                     ( [] )
# 331 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 60 "parser.mly"
                     ( _2 :: _1 )
# 339 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 66 "parser.mly"
     ( { fname = _1;
     formals = _3;
     locals = List.rev _6;
     body = List.rev _7 } )
# 352 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                      ( [] )
# 358 "parser.ml"
               : 'fdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 73 "parser.mly"
                      ( _2 :: _1 )
# 366 "parser.ml"
               : 'fdecl_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                  ( [] )
# 372 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 77 "parser.mly"
                  ( List.rev _1 )
# 379 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
                         ( [_1] )
# 386 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
                         ( _3 :: _1 )
# 394 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
              ( Expr(_1) )
# 401 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                     ( Return(_2) )
# 408 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 88 "parser.mly"
                            ( Block(List.rev _2) )
# 415 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 89 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 423 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 90 "parser.mly"
                                            ( If(_3, _5, _7) )
# 432 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 91 "parser.mly"
                                  ( While(_3, _5) )
# 440 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                   ( [] )
# 446 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 95 "parser.mly"
                   ( _2 :: _1 )
# 454 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
                  ( Noexpr )
# 460 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                  ( _1 )
# 467 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 104 "parser.mly"
                     ( Int_lit(_1) )
# 474 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 105 "parser.mly"
                     ( Double_lit(_1) )
# 481 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 106 "parser.mly"
                     ( Id(_1) )
# 488 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 107 "parser.mly"
                     ( String_lit(_1) )
# 495 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 108 "parser.mly"
                     ( Bool_lit(_1) )
# 502 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 510 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 518 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 526 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 534 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 542 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 550 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 558 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 566 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                     ( Binop(_1, Greater,  _3) )
# 574 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 582 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                     ( Assign(_1, _3) )
# 590 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 120 "parser.mly"
                                 ( Call(_1, _3) )
# 598 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                       ( _2 )
# 605 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
                  ( [] )
# 611 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 125 "parser.mly"
                  ( List.rev _1 )
# 618 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                            ( [_1] )
# 625 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                            ( _3 :: _1 )
# 633 "parser.ml"
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
