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

let find_func (fnames : string list) (f : string) = List.mem f fnames (* unnecessary function? *)

(*let rec check_fdecls (fnames : string list) = match fnames with
    [] -> print_endline "empty"; []
  | hd :: tl -> if List.mem hd tl then raise(Failure("function " ^ hd ^ " defined twice"))
      else print_endline "looking for keywords"; match hd with
        "main" -> raise(Failure("reserved function name \"main\""))
      | "print" -> raise(Failure("reserved function name \"print\""))
      | "draw" -> raise(Failure("reserved function name \"draw\""))
      | _ -> hd :: (check_fdecls tl)*)

let rec check_fdecls (prog : Ast.program) (fnames : string list ) (main_found : bool) = match prog with
    [] -> fnames
  | hd :: tl -> if List.mem hd.fname fnames then raise(Failure("function " ^ hd.fname ^ "() defined twice"))
      else match hd.fname with
          "print" -> raise(Failure "reserved function name 'print'")
        | "draw" -> raise(Failure "reserved function name 'draw'")
        | "main" -> if main_found then raise(Failure "main function can only be defined once")
            else check_fdecls tl fnames true
        | _ -> check_fdecls tl (hd.fname :: fnames) main_found

(* list printer for testing purposes *)
let rec print_list = function
    [] -> ()
  | hd :: tl -> print_endline hd; print_list tl

let check_program (prog : Ast.program) =
  (*let env = { scope = { vars = []; funcs = [];}; main_found = false } in
  check_stmt env (List.hd (prog)) (* this is def wrong *)*)
  if ((List.hd (prog)).fname <> "main") then raise(Failure "main function must be defined last") else
  let checked_fdecls = check_fdecls prog [] false in
  print_list checked_fdecls; print_endline "checked func decls!";

(* todo:
  + take out main function from the list passed to check_fdecls (so it doesn't throw an error for the actual main function)
  + 
*)










