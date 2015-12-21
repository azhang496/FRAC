%{ open Ast %}

%token SEMI COMMA COLON
%token LPAREN RPAREN LBRACE RBRACE
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token OR AND NOT
%token RETURN IF ELSE FOR WHILE
%token INT DOUBLE STRING BOOL
%token GRAM ALPHABET INIT RULES
%token LSQUARE RSQUARE ARROW QUOTE HYPHEN
%token RTURN LTURN MOVE
%token <string> ID
%token <int> INT_LIT
%token <float> DOUBLE_LIT
%token <string> STRING_LIT
%token <bool> BOOL_LIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT

%start program
%type <Ast.program> program

%%

program:
    /* nothing */      { [], [] }
  | program gdecl      { let (grams, funcs) = $1 in $2::grams, funcs }
  | program fdecl      { let (grams, funcs) = $1 in grams, $2::funcs }

/* VARIABLES */

var_type:
    INT    { Int }
  | DOUBLE { Double }
  | STRING { String }
  | BOOL   { Bool }
  | GRAM   { Gram }

vdecl:
    var_type ID SEMI              { Var($1, $2)}
  | var_type ID                   { Var($1, $2)}
  | var_type ID ASSIGN expr SEMI  { Var_Init($1, $2, $4)}

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

/* RULES */

rule_id_list:
    ID                     { [$1] }
  | rule_id_list ID  { $2 :: $1 }

comma_list:
    ID                     { [$1] }
  | comma_list COMMA ID  { $3 :: $1 }

rule:
    QUOTE ID QUOTE ARROW RTURN LPAREN expr RPAREN   { Term($2, Rturn($7)) }
  | QUOTE ID QUOTE ARROW LTURN LPAREN expr RPAREN   { Term($2, Lturn($7)) }
  | QUOTE ID QUOTE ARROW MOVE LPAREN expr RPAREN    { Term($2, Move($7)) }
  | QUOTE ID QUOTE ARROW QUOTE rule_id_list QUOTE   { Rec($2, List.rev $6) }

rule_list:
    rule                  { [$1] }
  | rule_list COMMA rule  { $3 :: $1 }

/* GRAMS */

gdecl:
    GRAM ID ASSIGN LBRACE
      ALPHABET COLON LSQUARE comma_list RSQUARE COMMA
      INIT COLON QUOTE rule_id_list QUOTE COMMA
      RULES COLON LBRACE rule_list RBRACE
    RBRACE
    { { gname = $2;
        alphabet = $8;
        init = $14;
        rules = List.rev $20 } }

 /* FUNCTIONS */

fdecl:
   ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $1;
     formals = $3;
     locals = List.rev $6;
     body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    vdecl                  { [] }
  | formal_list COMMA vdecl { $3 :: $1 }

/* STATEMENTS */

stmt:
    expr SEMI                                                     { Expr($1) }
  | RETURN expr SEMI                                              { Return($2) }
  | LBRACE stmt_list RBRACE                                       { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE                       { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt                          { If($3, $5, $7) }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt 	            { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt                                 { While($3, $5) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

/* EXPRESSIONS */

expr:
    INT_LIT          { Int_lit($1) }
  | DOUBLE_LIT       { Double_lit($1) }
  | ID               { Id($1) }
  | STRING_LIT       { String_lit($1) }
  | BOOL_LIT         { Bool_lit($1) }
  | LPAREN expr RPAREN { ParenExpr($2) }
  | NOT expr  		   { Unop(Not, $2) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MOD    expr { Binop($1, Mod,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
