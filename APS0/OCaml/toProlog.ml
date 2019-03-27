open Ast
let int_of_bool b = if b then 1 else 0


let rec print_prolog e =
	match e with
		ASTNum n -> Printf.printf"%d" n
	| ASTBool b ->  Printf.printf"%d" (int_of_bool b)	
	| ASTId x -> Printf.printf"\"%s\"" x
	| ASTPrim(op, e1, e2) -> (
	Printf.printf"%s" (string_of_op op);
	Printf.printf"(";
	print_prolog e1;
	Printf.printf",";
	print_prolog e2;
	Printf.printf")";
	)
	| ASTAlternative(cond,cons,alter) ->(
	Printf.printf"if( ";
	print_prolog cond;
	Printf.printf",";
	print_prolog cons;
	Printf.printf",";
	print_prolog alter;
	Printf.printf" )";
	)
	| ASTEcho(e) ->(
	Printf.printf"echo(";
	print_prolog e;
	Printf.printf")";
	)
	| ASTDec(i,t,e) ->(
	Printf.printf"declaration(";
	print_prolog i;
	Printf.printf",";
	print_prolog t;
	Printf.printf",";
	print_prolog e;
	Printf.printf")";
	)
	| ASTDecConst(i,t,e) ->(
	Printf.printf"declarationConst(";
	print_prolog i;
	Printf.printf",";
	print_prolog t;
	Printf.printf",";
	print_prolog e;
	Printf.printf")";
	)
	| ASTType(t) ->
	Printf.printf"\"%s\"" t
	|ASTArg(i,a) ->(
	Printf.printf "arg(";
	print_prolog i;
	Printf.printf ",";
	print_prolog a;
	Printf.printf ")";
	)
	|ASTSequenceType(t,ts) ->(
	Printf.printf "[";
	print_prolog t;
	Printf.printf "|";
	print_prolog ts;
	Printf.printf "]";
	)
	| ASTFunType(arg,res) ->(
	Printf.printf "typefun(";
	print_prolog arg;
	Printf.printf ",";
	print_prolog res;
	Printf.printf ")";
	)
	| ASTListArg(arg,args) ->(
	Printf.printf "[";
	print_prolog arg;
	Printf.printf "|";
	print_prolog args;
	Printf.printf "]";
	)
	| ASTFunAno(args,e) ->(
	Printf.printf "functionAnonyme(";
	print_prolog args;
	Printf.printf ",";
	print_prolog e;
	Printf.printf ")";
	)
	| ASTSequence(e1,e) ->(
	Printf.printf "[";
	print_prolog e1;
	Printf.printf "|";
	print_prolog e;
	Printf.printf "]";
	)
	| ASTFun(i,t,a,e) -> (
	Printf.printf "function(";
	print_prolog i;
	Printf.printf ",";
	print_prolog t;
	Printf.printf ",";
	print_prolog a;
	Printf.printf ",";
	print_prolog e;
	Printf.printf ")";
	)
	| ASTFunRec(i,t,a,e) -> (
	Printf.printf "functionRec(";
	print_prolog i;
	Printf.printf ",";
	print_prolog t;
	Printf.printf ",";
	print_prolog a;
	Printf.printf ",";
	print_prolog e;
	Printf.printf ")";
	)
	| ASTLog(op, e1, e2) -> (
	Printf.printf"%s" (string_of_op op);
	Printf.printf"(";
	print_prolog e1;
	Printf.printf",";
	print_prolog e2;
	Printf.printf")";
	)
	| ASTNot(op, e) -> (
	Printf.printf"%s" (string_of_op op);
	Printf.printf"(";
	print_prolog e;
	Printf.printf")";
	)
	| ASTCmds(instr,list_instr) ->(
	Printf.printf "[";
	print_prolog instr;
	Printf.printf "|";
	print_prolog list_instr;
	Printf.printf "]";
	)
	| ASTProg(cmds) ->(
	Printf.printf "prog(";
	print_prolog cmds;
	Printf.printf ")";
	)	
	
		
let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let e = Parser.prog Lexer.token lexbuf in
			print_prolog e;
			print_char '\n'
	with Lexer.Eof -> exit 0
