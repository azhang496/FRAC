open Ast
open Sast

let suffix_char s c = s ^ String.make 1 c

let c_print_types t = match t with
    Void    -> ""
  | Int     -> "\"%d\\n\""
  | Double  -> "\"%.2f\\n\""
  | String  -> "\"%s\\n\""
  | Boolean -> "\"%d\\n\""

let rec expr = function
    Int_lit(i) -> string_of_int i
  | Bool_lit(b) -> if b == true then "1" else "0"
  | Double_lit(d) -> if String.get (string_of_float d) (String.length (string_of_float d) - 1) == '.'
                        then suffix_char (string_of_float d) '0'
                     else string_of_float d
  | Id(decl) -> (match decl with
      Var(_, str) -> str
    | Var_Init(_, str, _) -> str)
  | String_lit(s) -> "\"" ^ s ^ "\""
  | ParenExpr((e,_)) -> "(" ^ (expr e) ^ ")"
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
  (* This DEFINITELY needs to be made more efficient *)
  | Call (fname, actuals) -> (match fname with
      "print" -> "printf(" ^
                 (let actuals_type = function
                   [] ->  ""
                   | (_,t)::[] -> c_print_types t
                   | _ -> ""
                   in actuals_type actuals)
                 ^ ", " ^
                 (let rec gen_actuals = function
                    [] ->  ""
                    | (e,_)::[] -> expr e
                    | _ -> ""
                    in gen_actuals actuals) ^ ")"
    | _       -> fname ^ "(" ^
                 (let rec gen_actuals = function
                    [] ->  ""
                    | (e,_)::[] -> expr e
                    | (e,_)::tl -> expr e ^ ", " ^ gen_actuals tl
                    in gen_actuals actuals) ^ ")")
  | Noexpr -> ""

let rec stmt = function
    Block sl -> String.concat "" (List.map stmt sl)
  | Expr (e,_) -> (expr e) ^ ";\n"
  | Return (e,_) -> "return " ^ (expr e) ^ ";\n"
  | If ((e,_), st, Block[]) -> "if(" ^ (expr e) ^ ") {\n" ^ (stmt st) ^ "}\n"
  | If ((e,_), st1, st2) -> "if(" ^ (expr e) ^ ") {\n" ^ (stmt st1) ^ "}\n" ^
                            "else" ^ "{\n" ^ (stmt st2) ^ "}\n"
  | For ((e1,_), (e2,_), (e3,_), st) -> "for(" ^ (expr e1) ^ "; " ^ (expr e2) ^ "; " ^ (expr e3) ^ ") {\n" ^ (stmt st) ^ "}\n2"
  | While ((e,_), st) -> "while(" ^ (expr e) ^ ") {\n" ^ (stmt st) ^ "}\n"

let rec gen_var_types = function
    Void -> "void "
  | Int -> "int "
  | Double -> "double "
	| String -> "char *"
	| Boolean -> "int "

let gen_formals v =
  let (str, var_decl, var_type) = v in match var_decl with
     Var(var_type, str) -> gen_var_types var_type ^ str
   | Var_Init(var_types, str, _) -> gen_var_types var_type ^ str

 let gen_locals v =
  let (str, var_decl, var_type) = v in match var_decl with
     Var(var_type, str) -> gen_var_types var_type ^ str
   | Var_Init(var_types, str, (e,_)) -> gen_var_types var_type ^ str ^ " = " ^ (expr e)

let rec gen_formals_list fl = match fl with
  [] -> ""
  | hd::[] -> gen_formals hd
  | hd::tl -> gen_formals hd ^ ", " ^ gen_formals_list tl

let rec gen_locals_list ll = match ll with
  [] -> ""
  | hd::[] -> gen_locals hd ^ ";\n"
  | hd::tl -> gen_locals hd ^ ";\n" ^ gen_locals_list tl

let gen_fdecl fdecl =
  (match fdecl.fname with
      "main" -> "int main()"
    | _      -> (match fdecl.rtype with
                    Sast.Void -> "void "
                  | Sast.Int -> "int "
                  | Sast.Double -> "double "
                  | Sast.String -> "char *"
                  | Sast.Boolean -> "int ")
  ^ fdecl.fname ^ "(" ^ (gen_formals_list fdecl.formals) ^ ")") ^ "{\n" ^(gen_locals_list fdecl.locals) ^ String.concat "" (List.map stmt fdecl.body) ^
  (match fdecl.fname with
      "main" -> "return 0;\n"
    | _      -> "" )
  ^ "}\n"

let generate funcs name =
  let outfile = open_out ("tests/" ^ name ^ "-NEW.c") in
  let translated_program =  "#include <stdio.h>\n" ^ String.concat "" (List.rev (List.map gen_fdecl funcs)) ^ "\n" in
  ignore(Printf.fprintf outfile "%s" translated_program);
  close_out outfile;