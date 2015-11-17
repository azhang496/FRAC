open Ast

let rec expr = function
    Literal i -> string_of_int i
  | Id s -> s
  | String s -> "\"" ^ s ^ "\""
  | Binop (e1, o, e2) -> (expr e1) ^ (match o with
        Add -> " + " | Sub -> " - " | Mult -> " * " | Div -> " / "
      | Equal -> " == " | Neq -> " != "
      | Less -> " < " | Leq -> " <= " | Greater -> " > " | Geq -> " >= "
    ) ^ (expr e2)
  | Assign (v, e) -> v ^ " = " ^ (expr e)
  | Call (fname, actuals) -> (match fname with
    "print" -> "printf(\"%s\", " ^ (expr (List.hd actuals)) ^ ")"
    | _     -> fname ^ "(" ^ (String.concat "," (List.map expr actuals)) ^ ")")
  | Noexpr -> ""

let rec stmt = function
    Block sl -> String.concat "" (List.map stmt sl)
  | Expr e -> (expr e) ^ ";\n"
  | Return e -> "return " ^ (expr e) ^ ";\n"
  | If (e, st, Block[]) -> "if(" ^ (expr e) ^ ") {\n" ^ (stmt st) ^ "}\n"
  | If (e, st1, st2) -> "if(" ^ (expr e) ^ ") {\n" ^ (stmt st1) ^ "}\n" ^
      "else" ^ "{\n" ^ (stmt st2) ^ "}\n"
  | While (e, st) -> "while(" ^ (expr e) ^ ") {\n" ^ (stmt st) ^ "}\n"

let add_type v = "type " ^ v

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
