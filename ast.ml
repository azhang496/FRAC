(* Binary operators *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

(* Expressions *)
type expr =
    Literal of int
  | Id of string
  | String of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

(* Statements *)
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | While of expr * stmt

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }

(* Program entry point *)
type program = func_decl list
