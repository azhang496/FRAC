open Ast
open Sast

type symbol_table = {
  vars: var_decl list;
  funcs: func_decl list;
}

(* list printer for testing purposes *)
let rec print_list = function
    [] -> ()
  | hd :: tl -> print_endline hd.fname; print_list tl

(*type environment = {
  scope: symbol_table;
}*)

(* idk if i will need this *)
(*let check_types t1 t2 =
  if (t1 != t2) then raise(Failure "type " ^ t2 ^ " but expected type " ^ t1)
  else print_endline "correct types!" (* should this be returning something instead of printing? *)*)

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
    Block(sl) -> Sast.Block(check_stmt_list sl)
  | Expr(e) -> Sast.Expr(check_expr e)
  | Return(e) -> Sast.Return(check_expr e)

and check_stmt_list (sl : Ast.stmt list) = match sl with
    [] -> []
  | hd :: tl -> (check_stmt hd) :: (check_stmt_list tl)

let find_rtype (sl : Ast.stmt list) = match sl with
    [] -> []
  | hd :: tl -> 

let sast_fdecl (f : Ast.func_decl) =
  { fname = f.fname; rtype = find_rtype f.body; formals = f.formals; locals = f.locals; body = (check_stmt_list f.body) }

(* returns an updated func_decl with return type *)
let check_fdecl (env : symbol_table) (f : Ast.func_decl) = match f.fname with
    "main" -> (match f.formals with
        [] -> sast_fdecl f
      | _ -> raise(Failure "main function cannot have formal parameters"))
  | _ -> sast_fdecl f

(* returns true if the function name is found in the current scope *)
let rec find_fname (funcs : func_decl list) (f : string) = match funcs with
    [] -> false
  | hd :: tl -> match hd.fname with
      fname when fname = f -> true
    | _ -> find_fname tl f

(* checks the list of function declarations in the program *)
let rec check_fdecl_list (env : symbol_table ) (prog : Ast.program) = match prog with
    hd :: [] -> if hd.fname <> "main" then raise(Failure "main function must be defined last") 
      else (*check_fdecl hd;*) env
  | hd :: tl -> if (find_fname env.funcs hd.fname) then raise(Failure("function " ^ hd.fname ^ "() defined twice"))
      else match hd.fname with
          "print" -> raise(Failure "reserved function name 'print'")
        | "draw" -> raise(Failure "reserved function name 'draw'")
        | "main" -> raise(Failure "main function can only be defined once")
        | _ -> check_fdecl_list ({ vars = env.vars; funcs = ((check_fdecl env hd) :: env.funcs); }) tl

(* entry point *)
let check_program (prog : Ast.program) =
  let env = { vars = []; funcs = [] } in
  let checked_fdecls = check_fdecl_list env (List.rev prog) in
  print_list checked_fdecls.funcs; print_endline "checked func decls!";




