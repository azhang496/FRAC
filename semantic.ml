open Ast
open Sast

type symbol_table = {
  vars: var_decl list;
  funcs: func_decl list;
}

type environment = {
  scope: symbol_table;
  main_found: bool;
}

(* dk if i will need this *)
let check_types t1 t2 =
  if (t1 != t2) then raise(Failure "type " ^ t2 ^ " but expected type " ^ t1)
  else print_endline "correct data types!"

let check_expr (scope: symbol_table) (expr: Ast.expr) = match expr with
    Int_lit(i) -> Sast.Int_lit(i)
  | Double_lit(f) -> Sast.Double_lit(f)
  | String_lit(s) -> Sast.String_lit(s)
  | Bool_lit(b) -> Sast.Bool_lit(b)
  | _ -> raise(Failure "invalid expression")

let rec check_stmt (scope: symbol_table) (stmt: Ast.stmt) = match stmt with
    Block(sl) -> Sast.Block(List.fold_left (fun a s -> (check_stmt scope s) :: a) [] sl)
  | Expr(e) -> Sast.Expr(check_expr scope e)
  | _ -> raise(Failure "invalid statement")

let check_program (prog: Ast.program) =
  let env = { scope = { vars = []; funcs = [];}; main_found = false } in
  List.map check_stmt prog (* this is def wrong *)
