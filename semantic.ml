(* 
todo:
  + syntax error in last line ?
  + 
*)

open Ast
open Sast

type symbol_table = {
  vars: var_decl list;
  funcs: func_decl list;
}

type environment = {
  scope: symbol_table;
}

(* idk if i will need this *)
(*let check_types t1 t2 =
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
  | stmt :: more_stmts -> (* CONTINUE WORKING ON THIS *)*)

let find_func (fnames : string list) (f : string) = List.mem f fnames

let rec check_fdecls (fnames : string list) = match fnames with
    hd -> hd
  | hd :: tl -> if find_func fnames hd then raise(Failure("function " ^ hd ^ " defined twice"))
      else match hd with
        "main" -> raise(Failure("reserved function name \"main\""))
      | "print" -> raise(Failure("reserved function name \"print\""))
      | "draw" -> raise(Failure("reserved function name \"draw\""))
      | _ -> hd :: (check_fdecls tl)
  (* check to see if the function has been defined already *)

let check_program (prog : Ast.program) =
  (*let env = { scope = { vars = []; funcs = [];}; main_found = false } in
  check_stmt env (List.hd (prog)) (* this is def wrong *)*)
  if ((List.hd (prog)).fname <> "main") then raise(Failure "no main function defined") else
  let checked_fdecls = check_fdecls (List.map (fun f -> f.fname) prog) in
  print_endline "checked func decls"












