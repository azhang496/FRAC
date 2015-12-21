open Ast
open Sast

let suffix_char s c = s ^ String.make 1 c

let c_print_types t = match t with
    Void    -> ""
  | Int     -> "\"%d\\n\""
  | Double  -> "\"%.2f\\n\""
  | String  -> "\"%s\\n\""
  | Boolean -> "\"%d\\n\""
  | Gram    -> raise(Failure "grams cannot be printed")

let rec gen_grow id n m str =
    if(n = 0) then str
    else if(n = m) then gen_grow id (n-1) m ("turtle_init(2000, 2000);\n" ^ id ^ "_start(" ^ (string_of_int n) ^ ");\nturtle_save_bmp(\""
         ^ id ^ (string_of_int n) ^ ".bmp\");\nturtle_cleanup()" ^ str)
    else gen_grow id (n-1) m ("turtle_init(2000, 2000);\n" ^ id ^ "_start(" ^ (string_of_int n) ^ ");\nturtle_save_bmp(\""
         ^ id ^ (string_of_int n) ^ ".bmp\");\nturtle_cleanup();\n" ^ str)

let rec expr = function
    Int_lit(i) -> string_of_int i
  | Bool_lit(b) -> if b == true then "1" else "0"
  | Double_lit(d) -> if String.get (string_of_float d) (String.length (string_of_float d) - 1) == '.'
                        then suffix_char (string_of_float d) '0'
                     else string_of_float d
  | Id(str) -> str
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
  | Assign (str, (e,_)) -> str ^ " = " ^ (expr e)
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
      | "draw" -> "turtle_init(2000, 2000);\n" ^
                (let [Sast.Id(s), Sast.Gram; Sast.Int_lit(n), Sast.Int] = actuals in
                s ^ "_start(" ^ (string_of_int n) ^ ");\nturtle_save_bmp(\"" ^ s ^ ".bmp\");\nturtle_cleanup()")
      | "grow" -> (let [Sast.Id(s), Sast.Gram; Sast.Int_lit(n), Sast.Int] = actuals in
                "char buf[1024];\nfor(int i = 0; i <" ^ (string_of_int n) ^ "; i++) {\nturtle_init(2000, 2000);\nmy_gram_start(i+1);"
                ^ "sprintf(buf, \"" ^ s ^ "%d.bmp\", i);\nturtle_save_bmp(buf);\nturtle_cleanup();\n}\n")
      | _       -> fname ^ "(" ^
                 (let rec gen_actuals = function
                    [] ->  ""
                    | (e,_)::[] -> expr e
                    | (e,_)::tl -> expr e ^ ", " ^ gen_actuals tl
                    in gen_actuals actuals) ^ ")")
  | Noexpr -> ""

let rec stmt = function
    Block sl -> String.concat "" (List.map stmt sl)
  | Expr (e,_) -> (match e with
                    Call(f, _) -> (match f with
                                    "grow" -> (expr e)
                                  | _      -> (expr e) ^ ";\n")
                  | _          -> (expr e) ^ ";\n")
  | Return (e,_) -> "return " ^ (expr e) ^ ";\n"
  | If ((e,_), st, Block[]) -> "if(" ^ (expr e) ^ ") {\n" ^ (stmt st) ^ "}\n"
  | If ((e,_), st1, st2) -> "if(" ^ (expr e) ^ ") {\n" ^ (stmt st1) ^ "}\n" ^
                            "else" ^ "{\n" ^ (stmt st2) ^ "}\n"
  | For ((e1,_), (e2,_), (e3,_), st) -> "for(" ^ (expr e1) ^ "; " ^ (expr e2) ^ "; " ^ (expr e3) ^ ") {\n" ^ (stmt st) ^ "}\n2"
  | While ((e,_), st) -> "while(" ^ (expr e) ^ ") {\n" ^ (stmt st) ^ "}\n"

let rec gen_var_types = function
	  Void    -> "void "
	| Int     -> "int "
  | Double  -> "double "
	| String  -> "char *"
	| Boolean -> "int "

let gen_formals v =
	(match v with
	   Var(var_type, str) -> gen_var_types var_type ^ str
	 | Var_Init(var_type, str, _) -> gen_var_types var_type ^ str)

 let gen_locals v =
  (match v with
 	   Var(var_type, str) -> gen_var_types var_type ^ str
 	 | Var_Init(var_type, str, (e,_)) -> gen_var_types var_type ^ str ^ " = " ^ (expr e))

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
                    Sast.Void    -> "void "
                  | Sast.Int     -> "int "
                  | Sast.Double  -> "double "
                  | Sast.String  -> "char *"
                  | Sast.Boolean -> "int ")
  ^ fdecl.fname ^ "(" ^ (gen_formals_list fdecl.formals) ^ ")") ^ "{\n" ^(gen_locals_list fdecl.locals) ^ String.concat "" (List.map stmt fdecl.body) ^
  (match fdecl.fname with
      "main" -> "return 0;\n"
    | _      -> "" )
  ^ "}\n"

(* list printer for testing purposes *)
let rec print_list = function
    [] -> ()
  | hd :: tl -> let Term(id, t) = hd in print_list tl

let rec divide_term_rules (tm, rtm) (recs : Sast.rule list) (terms : Sast.rule list) = match terms with
    [] -> print_list rtm; tm, rtm
  | hd :: tl -> let Term(id, t) = hd in if(List.exists (fun (Rec(s, _)) -> s = id) recs)
                then divide_term_rules (tm, hd :: rtm) recs tl
                else divide_term_rules (hd :: tm, rtm) recs tl

let gen_term_arg (e : Sast.expr) = match e with
    Int_lit(i) -> string_of_int i
  | Double_lit(d) -> string_of_float d

let rec gen_term_rules (terms : Sast.rule list) = match terms with
    [] -> ""
  | hd :: tl -> let Term(id, t) = hd in "if (var == '" ^ id ^ "') {\n" ^
                (match t with
                    Rturn(e) -> "turtle_turn_right(" ^ (gen_term_arg e) ^ ");\n"
                  | Lturn(e) -> "turtle_turn_left(" ^ (gen_term_arg e) ^ ");\n"
                  | Move(e) -> "turtle_forward(" ^ (gen_term_arg e) ^ ");\n"
                ) ^ "}\n" ^ gen_term_rules tl

let rec gen_init (gname : string) (rl : string list) = match rl with
    [] -> ""
  | hd :: tl -> gname ^ "('" ^ hd ^ "', iter);\n" ^ gen_init gname tl

let rec gen_rule (gname : string) (rl : string list) = match rl with
    [] -> ""
  | hd :: tl -> gname ^ "('" ^ hd ^ "', iter - 1);\n" ^ gen_rule gname tl

let rec gen_rec_rules (gname : string) (recs : Sast.rule list) = match recs with
    [] -> ""
  | hd :: tl -> let Rec(id, rl) = hd in "if(var == '" ^ id ^ "') {\n" ^ (gen_rule gname rl) ^ "}\n" ^ (gen_rec_rules gname tl)

let gen_gdecl (g : Sast.gram_decl) =
  let (terms, rterms) = divide_term_rules ([], []) g.rec_rules g.term_rules in
  "void " ^ g.gname ^ "(char var, int iter) {\n" ^ "if (iter < 0) {\n" ^
  (gen_term_rules rterms) ^ "} else {\n" ^ (gen_rec_rules g.gname g.rec_rules) ^ (gen_term_rules terms) ^ "}\n}\n" ^
  "void " ^ g.gname ^ "_start(int iter) {\n" ^ (gen_init g.gname (List.rev g.init)) ^ "}\n"

let generate (grams : Sast.gram_decl list) (funcs : Sast.func_decl list) (name : string) =
  let outfile = open_out ("tests/" ^ name ^ "-NEW.c") in
  let translated_program =  (if List.length grams > 0 then "#include \"turtle.h\"\n#include <string.h>\n" else "") ^ "#include <stdio.h>\n\n" ^
  String.concat "" (List.rev (List.map gen_gdecl grams)) ^ String.concat "" (List.rev (List.map gen_fdecl funcs)) ^ "\n" in
  ignore(Printf.fprintf outfile "%s" translated_program);
  close_out outfile;
