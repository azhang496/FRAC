open Ast
open Sast

type symbol_table = {
  mutable vars: (string * var_decl * var_type) list;
  mutable funcs: func_decl list;
}

(* list printer for testing purposes *)
let rec print_list = function
    [] -> ()
  | hd :: tl -> print_endline hd.fname; print_list tl

(* returns true if the function name is found in the current scope *)
let rec find_func (env : symbol_table) (f : string) =
  (try 
    List.find(fun func -> func.fname = f) env.funcs
  with Not_found -> raise(Failure ("function " ^ f ^ " not defined")))

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
  | Call(_, _) as c -> check_call env c
  | _ -> raise(Failure "invalid expression")

and check_id (env : symbol_table) id =
  let (_, decl, t) = List.find(fun (name, _, _) -> name = id) env.vars in
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

and check_call (env : symbol_table) c = match c with
  Ast.Call(f, actuals) -> (match f with
      "print" -> (match actuals with
          [arg] -> Sast.Call(f, [check_expr env arg]), Sast.Void
        | hd :: [_] -> raise(Failure "print() only takes one argument"))
    (*| "draw" -> Sast.Call(f, actuals), Sast.Void (* PLACEHOLDER: actuals should be gram g and int n *)*)
    | _ -> let called_func = find_func env f in
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
  let checked_locals = check_vdecl_list env f.locals in
  let new_env = { vars = checked_formals @ checked_locals; funcs = env.funcs } in
  { fname = f.fname; rtype = (find_rtype new_env f.body Sast.Void); formals = checked_formals; locals = checked_locals; body = (check_stmt_list new_env f.body) }

(* returns an updated func_decl with return type *)
let check_fdecl (env : symbol_table) (f : Ast.func_decl) = match f.fname with
    "main" -> (match f.formals with
        [] -> let sast_main = sast_fdecl env f in if (sast_main.rtype <> Sast.Void) then raise(Failure "main function should not return anything")
              else sast_main
      | _ -> raise(Failure "main function cannot have formal parameters"))
  | _ -> sast_fdecl env f

(* checks the list of function declarations in the program *)
let rec check_fdecl_list (env : symbol_table ) (prog : Ast.program) = match prog with
    hd :: [] -> if hd.fname <> "main" then raise(Failure "main function must be defined last") 
      else check_fdecl env hd; env
  | hd :: tl -> if (List.exists (fun func -> func.fname = hd.fname) env.funcs) then raise(Failure("function " ^ hd.fname ^ "() defined twice"))
      else match hd.fname with
          "print" -> raise(Failure "reserved function name 'print'")
        | "draw" -> raise(Failure "reserved function name 'draw'")
        | "main" -> raise(Failure "main function can only be defined once")
        | _ -> check_fdecl_list { vars = env.vars; funcs = (check_fdecl env hd) :: env.funcs } tl

(* entry point *)
let check_program (prog : Ast.program) =
  let env = { vars = []; funcs = [] } in
  let checked_fdecls = check_fdecl_list env (List.rev prog) in
  print_list checked_fdecls.funcs; print_endline "checked func decls!";