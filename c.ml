type expr =
    String of string 
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr

type func_decl = {
  fname: string;
  formals: string list;
  locals: string list;
  body: stmt list;
}

type program = string list * func_decl list

let rec string_of_expr = function
    String(s) -> '"' ^ s ^ '"'
  | Call(f, el) ->
    (match f with
        "print" -> "printf" ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
      | _       -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")" 
    )
  | Noexpr -> ""

let string_of_fdecl fdecl =
  (match fdecl.fname with
      "main" -> "int main"
    | _      -> fdecl.fname) ^
  "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
(*   String.concat "" (List.map string_of_vdecl fdecl.locals) ^ *)
  String.concat "" (List.map string_of_stmt fdecl.body) ^ "\n"
  (match fdecl.fname with
      "main" -> "return 0;\n"
    | _      -> "") ^
  "}\n"

let string_of_program (vars, funcs) =
(*   String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^ *)
  String.concat "" (List.map string_of_fdecl funcs)