open Ast
open Sast
(*
type symbol_table = {
  vars: var_decl list;
  funcs: func_decl list;
}

type environment = {
  scope: symbol_table;
}
*)


let rec check_expr (*(scope: symbol_table) *) (expr: Ast.expr) = match expr with
    Int_lit(i) -> Sast.Int_lit(i)
  | Double_lit(d) -> Sast.Double_lit(d)
  | String_lit(s) -> Sast.String_lit(s)
  | Bool_lit(b) -> Sast.Bool_lit(b)
  | Binop(e1, op, e2) -> check_binop e1 op e2
  | _ -> raise(Failure "invalid expression")

and check_binop e1 op e2 =
  let ew1 = check_expr e1 in
  let ew2 = check_expr e2 in
  match op with
    (* Numeric operations *)
    | Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq ->
        Sast.Binop(ew1, op, ew2)
    | _ -> raise (Failure "Incorrect stuff")

let rec check_stmt (s : Ast.stmt) = match s with
    Expr(e) -> Sast.Expr(check_expr e) (*
  | Return(e) -> check_expr e
  | Block(sl) -> check_stmt_list sl *)

and check_stmt_list (sl : Ast.stmt list) = match sl with
    [] -> []
  | hd :: tl -> (check_stmt hd) :: (check_stmt_list tl)

(*
(* returns the function name and return type *)
let check_fdecl (f : Ast.func_decl) = match f.fname with
    "main" -> match f.formals with
        [] -> check_stmt_list f.body (* PLACEHOLDER *)
      | _ -> raise(Failure "main function cannot have formal parameters")
  | _ -> check_stmt_list f.body
*)
let rec check_fdecl_list (prog : Ast.program) (fnames : string list ) = match prog with
    hd :: [] -> if hd.fname <> "main" then raise(Failure "main function must be defined last")
      else (*check_fdecl hd;*) fnames
  | hd :: tl -> if List.mem hd.fname fnames then raise(Failure("function " ^ hd.fname ^ "() defined twice"))
      else match hd.fname with
          "print" -> raise(Failure "reserved function name 'print'")
        | "draw" -> raise(Failure "reserved function name 'draw'")
        | "main" -> raise(Failure "main function can only be defined once")
        | _ -> (*check_fdecl hd;*) check_fdecl_list tl (hd.fname :: fnames)

(* list printer for testing purposes *)
let rec print_list = function
    [] -> ()
  | hd :: tl -> print_endline hd; print_list tl

let check_program (prog : Ast.program) =
  (*let env = { scope = { vars = []; funcs = [];}; main_found = false } in
  check_stmt env (List.hd (prog)) (* this is def wrong *)*)
  let checked_fdecls = check_fdecl_list (List.rev prog) [] in
  print_list checked_fdecls; print_endline "checked func decls!";

(* todo:
  + check that main func does not have args
  +
*)
