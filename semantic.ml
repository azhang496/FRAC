open Ast
open Sast

type symbol_table = {
  mutable vars: (string * var_decl_checked * var_type) list;
  mutable funcs: func_decl list;
}

(* list printer for testing purposes *)
let rec print_list = function
    [] -> ()
  | hd :: tl -> print_endline hd.fname; print_list tl

let rec check_expr (env : symbol_table) (expr : Ast.expr) = match expr with
    Noexpr -> Sast.Noexpr, Void
  | Id(str) ->
		(try
			let (decl, t) = check_id env str in Sast.Id(decl), t
		with Not_found -> raise (Failure ("Variable '" ^ str ^ "' not found")))
  | Int_lit(i) -> Sast.Int_lit(i), Sast.Int
  | Double_lit(d) -> Sast.Double_lit(d), Sast.Double
  | String_lit(s) -> Sast.String_lit(s), Sast.String
  | Bool_lit(b) -> Sast.Bool_lit(b), Sast.Boolean
  | Binop(_, _, _) as b -> check_binop env b
  | Assign(_, _) as a -> check_assign env a
  | _ -> raise(Failure "invalid expression")

and check_id (env : symbol_table) id =
	let (_, decl, t) = List.find(fun (n, _, _) -> n = id) env.vars in
	decl, t

and check_binop (env : symbol_table) binop = match binop with
	Ast.Binop(ex1, op, ex2) ->
		let e1 = check_expr env ex1 and e2 = check_expr env ex2 in
		let (_, t1) = e1 and (_, t2) = e2 in
		let t = match op with
			Add ->
				if (t1 <> Int || t2 <> Int) then
					if (t1 <> String || t2 <> String) then raise (Failure "Incorrect types for +")
					else String
				else Int
			| Sub -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for -") else Sast.Int
			| Mult -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for *") else Sast.Int
			| Div -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for /") else Sast.Int
			| Equal -> if (t1 <> t2) then raise (Failure "Incorrect types for = ") else Sast.Boolean
			| Neq -> if (t1 <> t2) then raise (Failure "Incorrect types for != ") else Sast.Boolean
			| Less -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for <") else Sast.Boolean
			| Leq -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for <=") else Sast.Boolean
			| Greater -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for >") else Sast.Boolean
			| Geq -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for >=") else Sast.Boolean
		in Sast.Binop(e1, op, e2), t
	| _ -> raise (Failure "Not a binary operator")

and check_assign (env : symbol_table) a = match a with
  Ast.Assign(id, expr) ->
    let (decl, t) = check_id env id in
    let e = check_expr env expr in
    let (_, t2) = e in
    if t <> t2 then raise (Failure "Incorrect type for assignment") else Sast.Assign(decl, e), t
  | _ -> raise (Failure "Not a valid assignment")

let check_vtype (t : Ast.var_type) = match t with
	  Int -> Sast.Int
  | Double -> Sast.Double
	| String -> Sast.String
	| Bool -> Sast.Boolean

let check_vdecl (env : symbol_table) (v : Ast.var_decl) =
	let declaration = match v with
		Var(t, name) ->
			let t = check_vtype t in
			(name, Sast.Var(t, name), t)
	| Var_Init(t, name, expr) ->
			let t = check_vtype t in
			let expr = check_expr env expr in
			let (_, t2 ) = expr in
			if t <> t2 then raise (Failure "Incorrect type for variable initialization") else (name, Sast.Var_Init(t, name, expr), t) in

  let (_, decl, t) = declaration in
	if t = Void then
		raise (Failure "Var cannot be type void.")
	else
		(env.vars <- declaration :: env.vars; (decl, t))


(*TODO*)
let check_stmt (env : symbol_table) (s : Ast.stmt) = match s with
    Expr(e) -> Sast.Expr(check_expr env e) (*
  | Return(e) -> check_expr e
  | Block(sl) -> check_stmt_list sl *)

let rec check_stmt_list (env : symbol_table) (sl : Ast.stmt list) = match sl with
    [] -> []
  | hd :: tl -> (check_stmt env hd) :: (check_stmt_list env tl)

let rec find_rtype (env : symbol_table) (body : Ast.stmt list) = match body with
    [] -> raise(Failure "function does not return anything")
  | hd :: tl -> (match hd with
      Return(e) -> let (_, t) = (check_expr env e) in t
    | _ -> find_rtype env tl)

let sast_fdecl (env : symbol_table) (f : Ast.func_decl) (r : Sast.var_type) =
  { fname = f.fname; rtype = r; formals = f.formals; locals = f.locals; body = (check_stmt_list env f.body) }

(* returns an updated func_decl with return type *)
let check_fdecl (env : symbol_table) (f : Ast.func_decl) = match f.fname with
    "main" -> (match f.formals with
        [] -> sast_fdecl env f Sast.Void
      | _ -> raise(Failure "main function cannot have formal parameters"))
  | _ -> sast_fdecl env f (find_rtype env f.body)

(* returns true if the function name is found in the current scope *)
let rec find_fname (funcs : func_decl list) (f : string) = match funcs with
    [] -> false
  | hd :: tl -> match hd.fname with
      fname when fname = f -> true
    | _ -> find_fname tl f

(* checks the list of function declarations in the program *)
let rec check_fdecl_list (env : symbol_table ) (prog : Ast.program) = match prog with
    hd :: [] -> if hd.fname <> "main" then raise(Failure "main function must be defined last")
      else check_fdecl env hd; env
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
