open Ast;;

let rec print_prolog e =
	match e with
	ASTNum n -> Printf.printf "%d" n
	| ASTId x -> Printf.printf "%s" x
	| ASTPrim(op,e1,e2) -> (
				Printf.printf "%s" (string_of_op op);
				Printf.printf "(";
				print_prolog e1;
				Printf.printf ",";
				print_prolog e2;
				Printf.printf ")";
				)
	| ASTUnaryPrim(op,e) -> (
				Printf.printf "%s" (string_of_op op);
				Printf.printf "(";
				print_prolog e;
				Printf.printf ")";
				)
	|ASTBool b -> Printf.printf "%s" (string_of_bool b)
	| ASTArg(id,t) -> (Printf.printf "(";
			print_prolog id;
			Printf.printf ",%s)" (string_of_type t) )
	| ASTArgs(arg,args) -> ( print_prolog arg;
				Printf.printf ",";
				print_prolog args )
	| ASTAbs(arg,e) -> ( Printf.printf "functionAnonyme([";
			print_prolog arg;
			Printf.printf "],";
			print_prolog e;
			Printf.printf ")" )
	|ASTSingle(e) -> print_prolog e;
	| ASTSequence(e1,e2) -> (print_prolog e1;
				Printf.printf ",";
				print_prolog e2 )
	| ASTApp(e1,e2) -> (Printf.printf "application(";
			print_prolog e1;
			Printf.printf ",[";
			print_prolog e2;
			Printf.printf "])" )
	| ASTIf(e1,e2,e3) -> ( Printf.printf "if(";
				print_prolog e1;
				Printf.printf ",";
				print_prolog e2;
				Printf.printf ",";
				print_prolog e3;
				Printf.printf ")" )
	| ASTEcho(e) -> ( Printf.printf "echo(";
			print_prolog e;
			Printf.printf ")"; )
	|ASTConst(i,t,e) -> ( Printf.printf "const(%s,%s," i (string_of_type t); 
			print_prolog e;
			Printf.printf ")")
	| ASTFun(i,t,a,e) -> ( Printf.printf "fun(%s,%s,[" i (string_of_type t);
			print_prolog a;
			Printf.printf "],";
			print_prolog e;
			Printf.printf ")" )
	| ASTFunRec(i,t,a,e) -> ( Printf.printf "funRec(%s,%s,[" i (string_of_type t);
			print_prolog a;
			Printf.printf "],";
			print_prolog e;
			Printf.printf ")" )
	| ASTDecs(dec,cmds) -> (print_prolog dec;
				Printf.printf ",";
				print_prolog cmds )
	| ASTStats(stat,cmds) -> (print_prolog stat;
				Printf.printf ",";
				print_prolog cmds )

let _=
	try
		let lexbuf = Lexing.from_channel stdin in
		let e =  Parser.cmds Lexer.token lexbuf in
		print_prolog e;
	with Lexer.Eof -> exit 0
