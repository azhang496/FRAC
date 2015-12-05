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
    Int_lit of int
  | Double_lit of float
  | Id of string
  | String_lit of string
  | Bool_lit of bool
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

(* Statements *)
type stmt =
    Expr of expr
  | Block of stmt list
  | Return of expr
  | If of expr * stmt * stmt
  | While of expr * stmt

(* Function Declarations *)
type func_decl = {
  fname : string;
  rtype: var_type;
  formals : string list;
  locals : var_decl list;
  body : stmt list;
}