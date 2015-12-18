open Ast
open Sast

module StringMap = Map.Make (String)

type symbol_table = {
  mutable vars: (string * var_decl * var_type) list;
  mutable funcs: func_decl list;
  mutable grams: gram_decl list;
}

(* list printer for testing purposes *)
let rec print_list = function
    [] -> ()
  | hd :: tl -> print_endline hd.fname; print_list tl

(**************
 * Exceptions *
**************)

exception Except of string

let op_error t = match t with
    Ast.Not -> raise (Except("Invalid use of unop: '!'"))
  | Ast.Add -> raise (Except("Invalid types for binop: '+'"))
  | Ast.Sub -> raise (Except("Invalid types for binop: '-'"))
  | Ast.Mult -> raise (Except("Invalid types for binop: '*'"))
  | Ast.Div -> raise (Except("Invalid types for binop: '/'"))
  | Ast.Mod -> raise (Except("Invalid types for binop: '%'"))
  | Ast.Or -> raise (Except("Invalid types for binop: '||'"))
  | Ast.And -> raise (Except("Invalid types for binop: '&&'"))
  | Ast.Equal -> raise (Except("Invalid types for binop: '=='"))
  | Ast.Neq -> raise (Except("Invalid types for binop: '!='"))
  | Ast.Less -> raise (Except("Invalid types for binop: '<'"))
  | Ast.Greater -> raise (Except("Invalid types for binop: '>'"))
  | Ast.Leq -> raise (Except("Invalid types for binop: '<='"))
  | Ast.Geq -> raise (Except("Invalid types for binop: '>='"))

(**************
 * Checking *
**************)

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
  | Unop(_, _) as u -> check_unop env u
  | Binop(_, _, _) as b -> check_binop env b
  | Assign(_, _) as a -> check_assign env a
  | Call(_, _) as c -> check_call env c

and check_id (env : symbol_table) (id : string) =
  let (_, decl, t) = List.find(fun (name, _, _) -> name = id) env.vars in
  decl, t

and check_unop (env : symbol_table) unop = match unop with
	Ast.Unop(op, e) ->
		(match op with
			Not ->
				let expr = check_expr env e in
				let (_, t) = expr in
				if (t <> Boolean)
          then op_error op
        else Sast.Unop(op, expr), t
			| _ -> raise (Failure "Invalid unary operator"))
	| _ -> raise (Failure "Invalid unary operator")

and check_binop (env : symbol_table) binop = match binop with
  Ast.Binop(ex1, op, ex2) ->
    let e1 = check_expr env ex1 and e2 = check_expr env ex2 in
    let (_, t1) = e1 and (_, t2) = e2 in
    let t = match op with
        Add ->
          if (t1 <> Int || t2 <> Int) then
            if (t1 <> Double || t2 <> Double) then
              if (t1 <> String || t2 <> String)
                then op_error op
              else Sast.String
            else Sast.Double
          else Sast.Int
      | Sub | Mult | Div | Mod ->
          if (t1 <> Int || t2 <> Int) then
            if (t1 <> Double || t2 <> Double)
              then op_error op
            else Sast.Double
          else Sast.Int
      | Greater | Less | Leq | Geq ->
          if (t1 <> Int || t2 <> Int) then
            if (t1 <> Double || t2 <> Double)
              then op_error op
            else Sast.Boolean
          else Sast.Boolean
      | And | Or ->
          if (t1 <> Boolean || t2 <> Boolean)
            then op_error op
          else Sast.Boolean
      | Equal | Neq ->
          if (t1 <> t2)
            then op_error op
          else Sast.Boolean
      | _ -> raise (Failure "Invalid binary operator")
    in Sast.Binop(e1, op, e2), t
  | _ -> raise (Failure "Not a binary operator")

and check_assign (env : symbol_table) a = match a with
  Ast.Assign(id, expr) ->
    let (decl, t) = check_id env id in
    let e = check_expr env expr in
    let (_, t2) = e in
    if t <> t2 then raise (Failure "Incorrect type for assignment") else Sast.Assign(decl, e), t
  | _ -> raise (Failure "Not a valid assignment")

and check_call (env : symbol_table) c = match c with
    Ast.Call(f, actuals) -> (match f with
        "print" -> (match actuals with
            []        -> raise(Failure "print() requires an argument")
          | hd :: []     -> Sast.Call(f, [check_expr env hd]), Sast.Void
          | hd :: tl -> raise(Failure "print() only takes one argument"))
      (*| "draw" -> Sast.Call(f, actuals), Sast.Void (* PLACEHOLDER: actuals should be gram g and int n *)*)
      | _ -> let called_func = (try
                List.find(fun func -> func.fname = f) env.funcs
              with Not_found -> raise(Failure ("function " ^ f ^ " not defined"))) in
             Sast.Call(f, (check_args env (called_func.formals, actuals))), called_func.rtype)
  | _ -> raise (Failure "Not a valid function call")

and check_args (env : symbol_table) ((formals : (string * var_decl * var_type) list), (actuals : Ast.expr list)) = match (formals, actuals) with
    ([], []) -> []
  | (f_hd :: f_tl, a_hd :: a_tl) ->
      let (_, _, f_type) = f_hd in
      let (a_expr, a_type) = check_expr env a_hd in
      if (f_type <> a_type) then raise (Failure "wrong argument type")
      else (a_expr, a_type) :: check_args env (f_tl, a_tl)
  | (_, _) -> raise (Failure "wrong number of arguments")

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

  let (_, _, t) = declaration in
  if t = Void then
    raise (Failure "Variables cannot be type void.")
  else declaration

let rec check_vdecl_list (env : symbol_table) (vl : Ast.var_decl list) = match vl with
    [] -> []
  | hd :: tl -> (check_vdecl env hd) :: (check_vdecl_list env tl)

let rec check_stmt (env : symbol_table) (s : Ast.stmt) = match s with
    Block(sl) -> Sast.Block(check_stmt_list env sl)
  | Expr(e) -> Sast.Expr(check_expr env e)
  | Return(e) -> Sast.Return(check_expr env e)
  | If(e, s1, s2) ->
		let expr = check_expr env e in
		let (_, t) = expr in
		if t <> Sast.Boolean then
			raise (Failure "If statement uses a boolean expression")
		else
			let stmt1 = check_stmt env s1 in
			let stmt2 = check_stmt env s2 in
			Sast.If(expr, stmt1, stmt2)
  | For(e1, e2, e3, s) ->
  	let ex1 = check_expr env e1 in
  	let ex2 = check_expr env e2 in
  	let (_, t) = ex2 in
  	if t <> Sast.Boolean then
  		raise (Failure "For statement uses a boolean expression")
  	else
  		let ex3 = check_expr env e3 in
  		let stmt = check_stmt env s in
  		Sast.For(ex1, ex2, ex3, stmt)
  | While(e, s) ->
		let expr = check_expr env e in
		let (_, t) = expr in
		if t <> Sast.Boolean then
			raise (Failure "While statement uses a boolean expression")
		else
			let stmt = check_stmt env s in
			Sast.While(expr, stmt)

and check_stmt_list (env : symbol_table) (sl : Ast.stmt list) = match sl with
    [] -> []
  | hd :: tl -> (check_stmt env hd) :: (check_stmt_list env tl)

let rec find_rtype (env : symbol_table) (body : Ast.stmt list) (rtype : Sast.var_type) = match body with
    [] -> rtype
  | hd :: tl -> (match hd with
      Return(e) -> if (rtype <> Sast.Void) then raise(Failure "function cannot have multiple return statements")
                   else let (_, t) = (check_expr env e) in find_rtype env tl t
    | _ -> find_rtype env tl rtype)

let sast_fdecl (env : symbol_table) (f : Ast.func_decl) =
  let checked_formals = check_vdecl_list env f.formals in
  let formals_env = { vars = env.vars @ checked_formals; funcs = env.funcs; grams = env.grams } in
  let checked_locals = check_vdecl_list formals_env f.locals in
  let new_env = { vars = formals_env.vars @ checked_locals; funcs = env.funcs; grams = env.grams } in
  { fname = f.fname; rtype = (find_rtype new_env f.body Sast.Void); formals = checked_formals; locals = checked_locals; body = (check_stmt_list new_env f.body) }

(* returns an updated func_decl with return type *)
let check_fdecl (env : symbol_table) (f : Ast.func_decl) = match f.fname with
    "main" -> (match f.formals with
        [] -> let sast_main = sast_fdecl env f in if (sast_main.rtype <> Sast.Void) then raise(Failure "main function should not return anything")
              else sast_main
      | _  -> raise(Failure "main function cannot have formal parameters"))
  | _ -> let checked_formals = check_vdecl_list env f.formals in sast_fdecl env f

(* checks the list of function declarations in the program *)
let rec check_fdecl_list (env : symbol_table ) (fdecls : Ast.func_decl list) = match fdecls with
    []       -> raise(Failure "Valid FRAC program must have at least a main function")
  | hd :: [] -> if hd.fname <> "main" then raise(Failure "main function must be defined last")
                else (check_fdecl env hd) :: env.funcs
  | hd :: tl -> if (List.exists (fun func -> func.fname = hd.fname) env.funcs) then raise(Failure("function " ^ hd.fname ^ "() defined twice"))
                else match hd.fname with
                    "print" -> raise(Failure "reserved function name 'print'")
                  | "draw" -> raise(Failure "reserved function name 'draw'")
                  | "main" -> raise(Failure "main function can only be defined once")
                  | _ -> check_fdecl_list { vars = env.vars; funcs = (check_fdecl env hd) :: env.funcs; grams = env.grams } tl

let rec check_alphabet (ids : Ast.expr list) (checked : Sast.expr list) = match ids with
    [] -> checked
  | hd :: tl -> let rule_id = (match hd with
                    Rule_id(c) -> Sast.Rule_id(c)
                  | _          -> raise(Failure "all rule names must be single characters")) in
                (* THIS LIST SEARCHING MIGHT NOT WORK *)
                if(List.mem rule_id checked) then raise(Failure "cannot have duplicates in alphabet")
                else check_alphabet tl (rule_id :: checked)

let rec check_init (a : Sast.expr list) (i : Ast.expr list) = match i with
    [] -> []
  | hd :: tl -> let rule_id = (match hd with
                  Rule_id(c) -> Sast.Rule_id(c)
                | _          -> raise(Failure "all rule names must be single characters")) in
                (try
                  let checked_id = List.find (fun r -> r = rule_id) a in check_init a i
                with Not_found -> raise(Failure "init contains a rule name not defined in alphabet"))

let rec check_rules (recs : Ast.rule list) (terms : Ast.rule list) (a : Sast.expr list) (rules : Ast.rule list) = match rules with
    []       -> []
  | hd :: tl -> (match hd with
                    Rec(s, rl) -> []
                  | Term(s, t) -> []
                )

let check_gdecl (env : symbol_table) (g : Ast.gram_decl) =
  if(List.exists (fun gram -> gram.gname = g.gname) env.grams) then raise(Failure "cannot define multiple grams with the same name")
  else let checked_alphabet = check_alphabet g.alphabet [] in
  let checked_init = check_init checked_alphabet g.init in
  let checked_rules = check_rules [] [] checked_alphabet g.rules in
  { gname = g.gname; alphabet = checked_alphabet; init = checked_init; rules = checked_rules }

let rec check_gdecl_list (env : symbol_table) (gdecls : Ast.gram_decl list) = match gdecls with
    [] -> []
  | hd :: tl -> if (List.exists (fun gram -> gram.gname = hd.gname) env.grams) then raise(Failure("gram " ^ hd.gname ^ "() defined twice"))
                (* PLACEHOLDER *)
                else hd :: (check_gdecl_list { vars = env.vars; funcs = env.funcs; grams = env.grams } tl)

(* entry point *)
let check_program (prog : Ast.program) =
  let (gdecls, fdecls) = prog in
  let env = { vars = []; funcs = []; grams = [] } in
  let checked_gdecls = check_gdecl_list env (List.rev gdecls) in (* PLACEHOLDER *)
  let checked_fdecls = check_fdecl_list env (List.rev fdecls) in
  checked_gdecls @ checked_fdecls
