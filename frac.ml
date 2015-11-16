type action = Ast | Compile

let _ =
(*   let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
            ("-c", Compile) ]
  else Compile in *)
  let input = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel input in
  let program = Parser.program Scanner.token lexbuf in
  Compile.generate program

(*   match action with
    Ast -> let listing = Ast.string_of_program program
           in print_string listing
  | Compile -> Compile.translate program *)
