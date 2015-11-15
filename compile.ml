open Ast

module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
(* type env = {
    function_index : int StringMap.t; (* Index for each function *)
    global_index   : int StringMap.t; (* "Address" for global variables *)
    local_index    : int StringMap.t; (* FP offset for args, locals *)
  } *)

let gen_fdecl fdecl =
  (match fdecl.fname with
      "main" -> "int main()"
    | _      -> "func " ^ fdecl.fname) ^
  "{\n" ^ String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"

let generate funcs =
  let rec expr = function
      Literal i -> i
    | Id s -> s
    | String s -> '"' ^ s '"'
    | Call (fname, actuals) -> (match fname with
        "print" -> "printf(" ^ actuals ^ ")"
      | _       -> fname ^ "(" ^ actuals ^ ")")
    | Noexpr -> ""

  in let rec stmt = function
      Block sl -> List.concat (List.map stmt sl)
    | Expr e -> expr e
