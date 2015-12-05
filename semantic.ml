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

(*NOT WORKING YET
let rec check_id (scope : symbol_table) id =
	let (_, decl, t) = List.find(fun (n, _, _) -> n = id ) scope.vars in
	decl, t
*)

let rec check_expr (*(scope: symbol_table) *) (expr: Ast.expr) = match expr with
    Noexpr -> Sast.Noexpr, Void
  | Id(str) ->
		(try
			let (decl, t) = check_id scope str in Sast.Id(decl), t
		with Not_found -> raise (Failure ("Id named " ^ str ^ " not found")))
  | Int_lit(i) -> Sast.Int_lit(i), Sast.Int
  | Double_lit(d) -> Sast.Double_lit(d), Sast.Double
  | String_lit(s) -> Sast.String_lit(s), Sast.String
  | Bool_lit(b) -> Sast.Bool_lit(b), Sast.Boolean
  | Binop(_, _, _) as b -> check_binop b
  | Assign(_, _) as a -> check_assign a
  | _ -> raise(Failure "invalid expression")

and check_binop binop = match binop with
	Ast.Binop(xp1, op, xp2) ->
		let e1 = check_expr xp1 and e2 = check_expr xp2 in
		let (_, t1) = e1 and (_, t2) = e2 in
		let t = match op with
			Add ->
				if (t1 <> Int || t2 <> Int) then
					if (t1 <> String || t2 <> String) then raise (Failure "Incorrect types for +")
					else String
				else Int
			| Sub -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for - ") else Sast.Int
			| Mult -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for * ") else Sast.Int
			| Div -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for / ") else Sast.Int
			| Equal -> if (t1 <> t2) then raise (Failure "Incorrect types for = ") else Sast.Boolean
			| Neq -> if (t1 <> t2) then raise (Failure "Incorrect types for != ") else Sast.Boolean
			| Less -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for < ") else Sast.Boolean
			| Leq -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for <= ") else Sast.Boolean
			| Greater -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for > ") else Sast.Boolean
			| Geq -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for >= ") else Sast.Boolean
		in Sast.Binop(e1, op, e2), t
	| _ -> raise (Failure "Not a binary operator")

(* NOT WORKING YET
and check_assign a = match a with
  Ast.Assign(id, expr) ->
    let (decl, t) = check_id id in
    let e = check_expr expr in
    let (_, t2) = e in
    if t <> t2 then raise (Failure "Incorrect type assignment.") else Sast.Assign(decl, e), t
  | _ -> raise (Failure "Not a valid assignment")
*)


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
