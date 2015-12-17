type action = Semantic | Compile

(* Get the name of the program from the file name. *)
let get_prog_name source_file_path =
	let split_path = (Str.split (Str.regexp_string "/") source_file_path) in
	let file_name = List.nth split_path ((List.length split_path) - 1) in
	let split_name = (Str.split (Str.regexp_string ".") file_name) in
		List.nth split_name ((List.length split_name) - 2)

let _ =
  let action =
		if Array.length Sys.argv > 2 then
    	List.assoc Sys.argv.(2) [ ("-s", Semantic); ("-c", Compile) ]
  	else Compile in
  let name = get_prog_name Sys.argv.(1) in
  let input = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel input in
  let program = Parser.program Scanner.token lexbuf in
	match action with
			Semantic -> let sprogram = Semantic.check_program program in
              print_string "Semantic in place" (* Pretty printer should go here. This is temp so that all patterns have unit return type *)
		| Compile	 -> Compile.generate (Semantic.check_program program) name
