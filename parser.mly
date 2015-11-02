%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE
%token ASSIGN EQ NEQ LT LEQ GT GEQ
%token FALSE TRUE
%token AND OR
%token RETURN IF ELSE WHILE INT
%token INT DOUBLE BOOL STRING
%token INIT RULES GRAM ARROW
%token FUNC
%token EOF
%token <int> LITERAL
%token <string> ID
%token <float> FLOAT

%nonassoc NOELSE /* Precedence and associativity of each operator */
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program /* Start symbol */
%type <Ast.program> program /* Type returned by a program */

%%

program:
decls EOF { $1 }
