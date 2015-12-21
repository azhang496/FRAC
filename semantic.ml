open Ast
open Sast

module StringMap = Map.Make (String)

type symbol_table = {
  mutable vars: var_decl list;
  mutable funcs: func_decl list;
  mutable grams: gram_decl list;
}

(**************
 * Exceptions *
**************)

exception Failure of string

let op_error t = match t with
    Ast.Not -> raise (Failure("Invalid use of unop: '!'"))
  | Ast.Add -> raise (Failure("Invalid types for binop: '+'"))
  | Ast.Sub -> raise (Failure("Invalid types for binop: '-'"))
  | Ast.Mult -> raise (Failure("Invalid types for binop: '*'"))
  | Ast.Div -> raise (Failure("Invalid types for binop: '/'"))
  | Ast.Mod -> raise (Failure("Invalid types for binop: '%'"))
  | Ast.Or -> raise (Failure("Invalid types for binop: '||'"))
  | Ast.And -> raise (Failure("Invalid types for binop: '&&'"))
  | Ast.Equal -> raise (Failure("Invalid types for binop: '=='"))
  | Ast.Neq -> raise (Failure("Invalid types for binop: '!='"))
  | Ast.Less -> raise (Failure("Invalid types for binop: '<'"))
  | Ast.Greater -> raise (Failure("Invalid types for binop: '>'"))
  | Ast.Leq -> raise (Failure("Invalid types for binop: '<='"))
  | Ast.Geq -> raise (Failure("Invalid types for binop: '>='"))

(**************
 * Checking *
**************)

let rec check_expr (env : symbol_table) (expr : Ast.expr) = match expr with
    Noexpr -> Sast.Noexpr, Void
  | Id(str) -> (match (find_vname str env.vars) with
                  Var(vt, s) -> Sast.Id(s), vt
                | Var_Init(vt, s, e) -> Sast.Id(s), vt)
  | Int_lit(i) -> Sast.Int_lit(i), Sast.Int
  | Double_lit(d) -> Sast.Double_lit(d), Sast.Double
  | String_lit(s) -> Sast.String_lit(s), Sast.String
  | Bool_lit(b) -> Sast.Bool_lit(b), Sast.Boolean
  | ParenExpr(e) -> check_paren_expr env e
  | Unop(_, _) as u -> check_unop env u
  | Binop(_, _, _) as b -> check_binop env b
  | Assign(_, _) as a -> check_assign env a
  | Call(_, _) as c -> check_call env c

and check_paren_expr (env : symbol_table) pe =
  let e = check_expr env pe in
  let (_, t) = e in
  Sast.ParenExpr(e), t

and find_vname (vname : string) (vars : Sast.var_decl list) = match vars with
    [] -> raise(Failure "variable not defined")
  | hd :: tl -> let name = (match hd with
                              Var(vt, s) -> s
                            | Var_Init(vt, s, e) -> s) in
                if(vname = name) then hd
                else find_vname vname tl

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
        Mod ->
          if (t1 <> Int || t2 <> Int)
                then op_error op
          else Sast.Int
      | Add | Sub | Mult | Div ->
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
          if (t1 <> Int || t2 <> Int) then
            if (t1 <> Double || t2 <> Double) then
              if (t1 <> Boolean || t2 <> Boolean)
                then op_error op
              else Sast.Boolean
            else Sast.Boolean
          else Sast.Boolean
      | _ -> raise (Failure "Invalid binary operator")
    in Sast.Binop(e1, op, e2), t
  | _ -> raise (Failure "Not a binary operator")

and check_assign (env : symbol_table) a = match a with
  Ast.Assign(id, expr) ->
    let vdecl = find_vname id env.vars in
    let (t,n) = (match vdecl with
              Var(vt, s) -> (vt,s)
            | Var_Init(vt, s, e) -> (vt,s)) in
    let e = check_expr env expr in
    let (_, t2) = e in
    if t <> t2 then raise (Failure "Incorrect type for assignment") else Sast.Assign(n, e), t
  | _ -> raise (Failure "Not a valid assignment")

and check_call (env : symbol_table) c = match c with
    Ast.Call(f, actuals) -> (match f with
        "print" -> (match actuals with
            []        -> raise(Failure "print() requires an argument")
          | hd :: []     -> let (id, t) = check_expr env hd in (match t with
                                Sast.Void -> raise(Failure "cannot print an expression of type void")
                              | _ -> Sast.Call(f, [(id, t)]), Sast.Void)
          | hd :: tl -> raise(Failure "print() only takes one argument"))
      | "draw" -> (match actuals with
            [g; i] -> (match (g, i) with
                          (Id(s), Int_lit(n)) -> ignore(try
                                                  List.find(fun gram -> gram.gname = s) env.grams
                                                  with Not_found -> raise(Failure ("gram " ^ s ^ " not defined")));
                          Sast.Call(f, [Sast.Id(s), Sast.Gram; Sast.Int_lit(n), Sast.Int]), Sast.Void
                        | _ -> raise(Failure "draw takes a gram g and int n as arguments"))
          | _      -> raise(Failure "draw() requires two arguments"))
      | "grow" -> (match actuals with
            [g; i] -> (match (g, i) with
                          (Id(s), Int_lit(n)) -> ignore(try
                                                  List.find(fun gram -> gram.gname = s) env.grams
                                                  with Not_found -> raise(Failure ("gram " ^ s ^ " not defined")));
                          Sast.Call(f, [Sast.Id(s), Sast.Gram; Sast.Int_lit(n), Sast.Int]), Sast.Void
                        | _ -> raise(Failure "grow takes a gram g and int n as arguments"))
          | _      -> raise(Failure "draw() requires two arguments"))
      | _ -> let called_func = (try
                List.find(fun func -> func.fname = f) env.funcs
                with Not_found -> raise(Failure ("function " ^ f ^ " not defined"))) in
             Sast.Call(f, (check_args env (called_func.formals, actuals))), called_func.rtype)
  | _ -> raise (Failure "Not a valid function call")

and check_args (env : symbol_table) ((formals : var_decl list), (actuals : Ast.expr list)) = match (formals, actuals) with
    ([], []) -> []
  | (f_hd :: f_tl, a_hd :: a_tl) ->
      let f_type = (match f_hd with
                Var(t, _) -> t
              | Var_Init(t, _, _) -> t) in
      let (a_expr, a_type) = check_expr env a_hd in
                             if (f_type <> a_type) then raise (Failure "wrong argument type")
                             else (a_expr, a_type) :: check_args env (f_tl, a_tl)
  | (_, _) -> raise (Failure "wrong number of arguments")

let check_vtype (t : Ast.var_type) = match t with
    Int    -> Sast.Int
  | Double -> Sast.Double
  | String -> Sast.String
  | Bool   -> Sast.Boolean
  | Gram   -> Sast.Gram
  | _      -> raise (Failure "Variables cannot be of this type.")

let rec check_dup_vdecl (vname : string) (vars : Sast.var_decl list) = match vars with
    [] -> vname
  | hd :: tl -> (match hd with
                    Var(_, name) -> if(name = vname) then raise(Failure ("variable " ^ vname ^ " already declared"))
                                    else check_dup_vdecl vname tl
                  | Var_Init(_, name, _) -> if(name = vname) then raise(Failure ("variable " ^ vname ^ " already declared"))
                                            else check_dup_vdecl vname tl
                )

let check_vdecl (env : symbol_table) (v : Ast.var_decl) =
  (match v with
    Var(t, name) ->
      ignore(check_dup_vdecl name env.vars);
      let t = check_vtype t in Sast.Var(t, name)
  | Var_Init(t, name, expr) ->
      ignore(check_dup_vdecl name env.vars);
      let t = check_vtype t in
      let expr = check_expr env expr in
      let (_, t2 ) = expr in
      if t <> t2 then raise (Failure "Incorrect type for variable initialization") else Sast.Var_Init(t, name, expr))

let rec check_vdecl_list (env : symbol_table) (vl : Ast.var_decl list) = match vl with
    [] -> []
  | hd :: tl -> let checked_vdecl = check_vdecl env hd in
                checked_vdecl :: (check_vdecl_list { vars = (checked_vdecl :: env.vars); funcs = env.funcs; grams = env.grams } tl)

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
  | _ -> sast_fdecl env f

(* checks the list of function declarations in the program *)
let rec check_fdecl_list (env : symbol_table ) (fdecls : Ast.func_decl list) = match fdecls with
    []       -> raise(Failure "Valid FRAC program must have at least a main function")
  | hd :: [] -> if hd.fname <> "main" then raise(Failure "main function must be defined last")
                else (check_fdecl env hd) :: env.funcs
  | hd :: tl -> if (List.exists (fun func -> func.fname = hd.fname) env.funcs) then raise(Failure("function " ^ hd.fname ^ "() defined twice"))
                else match hd.fname with
                    "print" -> raise(Failure "reserved function name 'print'")
                  | "draw"  -> raise(Failure "reserved function name 'draw'")
                  | "grow"  -> raise(Failure "reserved function name 'grow'")
                  | "main"  -> raise(Failure "main function can only be defined once")
                  | _ -> check_fdecl_list { vars = env.vars; funcs = (check_fdecl env hd) :: env.funcs; grams = env.grams } tl

let rec find_rule (id : string) (rules : Ast.rule list) = match rules with
    [] -> raise(Failure "all elements of the alphabet must have corresponding rules")
  | hd :: tl -> (match hd with
                    Rec(c, rl) -> if(c = id) then c
                                  else find_rule id tl
                  | Term(c, t) -> if(c = id) then c
                                  else find_rule id tl)

let rec check_alphabet (checked : string list) (rules : Ast.rule list) (a : string list) = match a with
    [] -> []
  | hd :: tl -> if(List.mem hd checked) then raise(Failure "cannot have duplicates in alphabet")
                else let checked_c = find_rule hd rules in
                checked_c :: (check_alphabet (checked_c :: checked) rules tl)

let rec check_rule (a : string list) (i : string list) = match i with
    [] -> []
  | hd :: tl -> ignore(try List.find (fun id -> id = hd) a with Not_found -> raise(Failure "contains a rule not found in alphabet"));
                hd :: (check_rule a tl)

let check_turn_expr (e : Ast.expr) = match e with
    Int_lit(i) -> Sast.Int_lit(i)
  | Double_lit(d) -> Sast.Double_lit(d)
  | _ -> raise(Failure "turn functions must have argument of type int or double")

let check_move_expr (e : Ast.expr) = match e with
    Int_lit(i) -> Sast.Int_lit(i)
  | _ -> raise(Failure "turn functions must have argument of type int or double")

let rec check_rules (recs : Sast.rule list) (terms : Sast.rule list) (a : string list) (rules : Ast.rule list) = match rules with
    []       -> recs, terms
  | hd :: tl -> (match hd with
                    Rec(c, rl) -> ignore(try List.find (fun id -> id = c) a with Not_found -> raise(Failure "rule not found in alphabet"));
                      ignore(if(List.exists (fun (rl : Sast.rule) -> match rl with
                          Rec(id, _) -> if(id = c) then true else false
                        | Term(_, _) -> false) recs) then raise(Failure "multiple recursive rules of the same name")
                      else check_rule a rl); let checked_rec = Sast.Rec(c, rl) in
                      check_rules (checked_rec :: recs) terms a tl
                  | Term(c, t) -> ignore(try List.find (fun id -> id = c) a with Not_found -> raise(Failure "rule not found in alphabet"));
                      if(List.exists (fun (t : Sast.rule) -> match t with
                          Term(id, _) -> if(id = c) then true else false
                        | Rec(_, _) -> false) terms) then raise(Failure "multiple terminal rules of the same name")
                      else let checked_t = (match t with
                          Rturn(e) -> Sast.Rturn(check_turn_expr e)
                        | Lturn(e) -> Sast.Lturn(check_turn_expr e)
                        | Move(e) -> Sast.Move(check_move_expr e)) in
                      let checked_term = Sast.Term(c, checked_t) in
                      check_rules recs (checked_term :: terms) a tl
                )

let check_gdecl (g : Ast.gram_decl) =
  let checked_alphabet = check_alphabet [] g.rules g.alphabet in
  let (checked_recs, checked_terms) = check_rules [] [] checked_alphabet g.rules in
  let checked_init = check_rule checked_alphabet g.init in
  { gname = g.gname; alphabet = checked_alphabet; init = checked_init; rec_rules = checked_recs; term_rules = checked_terms }

let rec check_gdecl_list (checked_gdecls : Sast.gram_decl list) (gdecls : Ast.gram_decl list) = match gdecls with
    [] -> checked_gdecls
  | hd :: tl -> if (List.exists (fun gram -> gram.gname = hd.gname) checked_gdecls) then raise(Failure("gram " ^ hd.gname ^ " defined twice"))
                else check_gdecl_list ((check_gdecl hd) :: checked_gdecls) tl

(* entry point *)
let check_program (prog : Ast.program) =
  let (gdecls, fdecls) = prog in
  let env = { vars = []; funcs = []; grams = [] } in
  let checked_gdecls = check_gdecl_list [] (List.rev gdecls) in
  let grams_env = { vars = env.vars; funcs = env.funcs; grams = checked_gdecls } in
  let checked_fdecls = check_fdecl_list grams_env (List.rev fdecls) in
  checked_gdecls, checked_fdecls
