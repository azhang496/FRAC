(* Binary operators *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq


(* Variable types *)
type var_type =
    Int
  | Double
  | String
  | Bool

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

(* Variable Declarations *)

type var_decl = {
  vtype : var_type;
  vname : string;
  value : expr;
}

(* Function Declarations *)

type func_decl = {
  fname : string;
  formals : string list;
  locals : var_decl list;
  body : stmt list;
}

(*Rules

type rule =
    Rec of string * string
  | Term of string * expr

Grammars

type gram = {
  gname : string;
  init : string;
  rules : rule list;
} *)

(* Program entry point *)
type program = func_decl list
