open Ast
open Sast

let rec expr = function
    Int_lit(i) -> string_of_int i
  | Id(decl) -> (match decl with
		  Var(_, str) -> str
		| Var_Init(_, str, _) -> str)
  | String_lit(s) -> "\"" ^ s ^ "\""
  | Unop(op, (e,_)) -> (match op with
        Not -> " ! "
      | _   -> ""
    ) ^ (expr e)
  | Binop ((e1,_), op, (e2,_)) -> (expr e1) ^ (match op with
        Add     -> " + "
      | Sub     -> " - "
      | Mult    -> " * "
      | Div     -> " / "
      | Mod     -> " % "
      | Equal   -> " == "
      | Neq     -> " != "
      | Less    -> " < "
      | Leq     -> " <= "
      | Greater -> " > "
      | Geq     -> " >= "
      | And     -> " && "
      | Or      -> " || "
      | _       -> ""
    ) ^ (expr e2)
  | Assign (decl, (e,_)) -> (match decl with
		  Var(_, str) -> str
		| Var_Init(_, str, _) -> str) ^ " = " ^ (expr e)


    (* HAVING TROUBLE WITH CALL FUNCTION *) (*
  | Call (fname, actuals) -> (match fname with
    "print" -> "printf(\"%s\", " ^ (expr (List.hd actuals)) ^ ")"
    | _     -> fname ^ "(" ^ (String.concat "," (List.map expr actuals)) ^ ")") *)
  | Noexpr -> ""

let rec stmt = function
    Block sl -> String.concat "" (List.map stmt sl)
  | Expr (e,_) -> (expr e) ^ ";\n"
  | Return (e,_) -> "return " ^ (expr e) ^ ";\n"
  | If ((e,_), st, Block[]) -> "if(" ^ (expr e) ^ ") {\n" ^ (stmt st) ^ "}\n"
  | If ((e,_), st1, st2) -> "if(" ^ (expr e) ^ ") {\n" ^ (stmt st1) ^ "}\n" ^
      "else" ^ "{\n" ^ (stmt st2) ^ "}\n"
  | While ((e,_), st) -> "while(" ^ (expr e) ^ ") {\n" ^ (stmt st) ^ "}\n"

let add_type v = "type " ^ v


(* HAVING TROUBLE HERE TOO *)
let gen_fdecl fdecl =
  (match fdecl.fname with
      "main" -> "int main()"
    | _      -> "return_type " ^ fdecl.fname ^ "(" ^ (String.concat ", " (List.map add_type fdecl.formals)) ^ ")") ^
"{\n" ^ String.concat "" (List.map stmt fdecl.body) ^
  (match fdecl.fname with
      "main" -> "return 0;\n"
    | _      -> "" ) ^ "}\n"

let generate funcs name =
  let outfile = open_out ("tests/" ^ name ^ "-NEW.c")
  in let translated_program =  "#include <stdio.h>\n" ^ String.concat "" (List.rev (List.map gen_fdecl funcs)) ^ "\n"
  in ignore(Printf.fprintf outfile "%s" translated_program);
  close_out outfile;
