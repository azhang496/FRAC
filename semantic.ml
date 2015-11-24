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

(* idk if i will need this *)
let check_types t1 t2 =
  if (t1 != t2) then raise(Failure "type " ^ t2 ^ " but expected type " ^ t1)
  else print_endline "correct types!" (* should this be returning something instead of printing? *)

let check_expr (scope: symbol_table) (expr: Ast.expr) = match expr with
    Int_lit(i) -> Sast.Int_lit(i)
  | Double_lit(d) -> Sast.Double_lit(d)
  | String_lit(s) -> Sast.String_lit(s)
  | Bool_lit(b) -> Sast.Bool_lit(b)
  | _ -> raise(Failure "invalid expression")

let rec check_stmt (scope: symbol_table) (stmt: Ast.stmt) = match stmt with
    Block(sl) -> Sast.Block(List.fold_left (fun a s -> (check_stmt scope s) :: a) [] sl)
  | Expr(e) -> Sast.Expr(check_expr scope e)
  | _ -> raise(Failure "invalid statement")

and check_stmts (scope: symbol_table) stmts = match stmts with
    [] -> scope
  | [stmt] -> check_stmt scope stmt
  | stmt :: more_stmts -> (* CONTINUE WORKING ON THIS *)

let check_program (prog: Ast.program) =
  let env = { scope = { vars = []; funcs = [];}; main_found = false } in
  check_stmt env List.hd (prog) (* this is def wrong *)