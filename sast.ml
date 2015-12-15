open Ast

(* Variable types *)
type var_type =
    Void
  | Int
  | Double
  | String
  | Boolean

(* Variable Declarations *)
and var_decl_checked =
    Var of string * var_type

and fdecl = {
	ftype: var_type;
	fname : string; (* Name of the function *)
  formals : string list;
  locals : var_decl list;
	body : stmt list;
}

(* Expressions *)
and expr =
    Noexpr
  | Int_lit of int
  | Double_lit of float
  | Id of var_decl_checked
  | String_lit of string
  | Bool_lit of bool
  | Unop of op * expression
  | Binop of expression * op * expression
  | Assign of var_decl_checked * expression
  | Call of string * expression list
  | Fdecl of fdecl

and expression = expr * var_type

(* Statements *)
and stmt =
    Expr of expression
  | Block of stmt list
  | Return of expression
  | If of expression * stmt * stmt
  | While of expression * stmt


type program = fdecl list
