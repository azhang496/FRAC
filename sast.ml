open Ast

(* Variable types *)
type var_type =
    Void
  | Int
  | Double
  | String
  | Boolean

(* Expressions *)
type expr =
    Noexpr
  | Int_lit of int
  | Double_lit of float
  | Id of string
  | String_lit of string
  | Bool_lit of bool
  | Unop of op * expression
  | Binop of expression * op * expression
  | Assign of string * expression
  | Call of string * expression list

and expression = expr * var_type

(* Statements *)
type stmt =
    Expr of expression
  | Block of stmt list
  | Return of expression
  | If of expression * stmt * stmt
  | While of expression * stmt

(* Function Declarations *)
type func_decl = {
  fname : string;
  rtype: var_type;
  formals : string list;
  locals : var_decl list;
  body : stmt list;
}