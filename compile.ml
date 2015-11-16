open Ast

module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
(* type env = {
    function_index : int StringMap.t; (* Index for each function *)
    global_index   : int StringMap.t; (* "Address" for global variables *)
    local_index    : int StringMap.t; (* FP offset for args, locals *)
  } *)

let rec expr = function
    Literal i -> string_of_int i
  | Id s -> s
  | String s -> "\"" ^ s ^ "\""
  | Call (fname, actuals) -> (match fname with
    "print" -> "printf(\"%s\", " ^ (expr (List.hd actuals)) ^ ")"
(*         "print" -> "printf(" ^ expr actuals ^ ")" *)
(*       | _       -> fname ^ "(" ^ expr actuals ^ ")") *)
    | _     -> "")
  | Noexpr -> ""

let rec stmt = function
    Block sl -> String.concat "" (List.map stmt sl)
  | Expr e -> expr e ^ ";\n"

let gen_fdecl fdecl =
  (match fdecl.fname with
      "main" -> "int main()"
    | _      -> "func " ^ fdecl.fname) ^
"{\n" ^ String.concat "" (List.map stmt fdecl.body) ^
  (match fdecl.fname with
      "main" -> "return 0;\n"
    | _      -> "" ) ^ "}"

let generate funcs =
  let outfile = open_out "test.c"
  in let translated_program =  "#include <stdio.h>\n" ^ String.concat "" (List.map gen_fdecl funcs) ^ "\n"
  in ignore(Printf.fprintf outfile "%s" translated_program);
  close_out outfile;
