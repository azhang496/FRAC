open Ast

(* Variable types *)
type var_type =
    Int
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

(* Statements *)
type stmt =
    Expr of expr
  | Block of stmt list
  | Return of expr
  | If of expr * stmt * stmt
  | While of expr * stmt