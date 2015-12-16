open Ast

(* Variable types *)
type var_type =
    Void
  | Int
  | Double
  | String
  | Boolean

(* Variable Declarations*)
and var_decl =
    Var of var_type * string
  | Var_Init of var_type * string * expression

(*
and var_decl = {
  vtype : var_type;
  vname : string;
  value : expr;
}
*)

(* Function Declarations *)
and func_decl = {
  fname: string;
  rtype: var_type;
  formals: (var_decl * var_type) list;
  locals: (var_decl * var_type) list;
  body: stmt list;
}

(* Expressions *)
and expr =
    Noexpr
  | Int_lit of int
  | Double_lit of float
  | Id of var_decl
  | String_lit of string
  | Bool_lit of bool
  | Unop of op * expression
  | Binop of expression * op * expression
  | Assign of var_decl * expression
  | Call of string * expression list

and expression = expr * var_type

(* Statements *)
and stmt =
    Expr of expression
  | Block of stmt list
  | Return of expression
  | If of expression * stmt * stmt
  | While of expression * stmt


type program = func_decl list
