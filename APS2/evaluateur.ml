open Ast;;

type valeur = InN of int
			| InF of expr * string list * (string * valeur) list
			| InFR of string * valeur
			(*APS1*)
			| InA of int
			| InP of block * string list * (string * valeur) list
			| InPR of string * valeur
			(*APS1*)

(*APS1*)
let cpt = ref 0
let alloc mem =
	let res = (!cpt,(!cpt,ref (InN(-1)))::mem) in
		cpt:=(!cpt+1);
		res


let get_int value =
	match value with
	|InN(n) -> n
	| _ -> failwith "not in N"

let get_string v =
	match v with
	| InN(e) -> string_of_int e
	|_ -> failwith "get_string fail"
(*APS1*)


let eval_opbin op e1 e2 =
	match op with
	| "and" -> if e1 = 0 then 0 else e2
	| "or" -> if e1 = 1 then 1 else e2
	| "eq" ->  if e1 = e2 then 1 else 0
	| "lt" ->  if e1 < e2 then 1 else 0
	| "add" -> e1+e2
	| "sub" -> e1-e2
	| "mul" -> e1*e2
	| "div" -> e1/e2
	| _ -> failwith " opbin fail"


let eval_opun op e =
	match op with
	| "not" -> if e = 0 then 1 else 0
	| _ -> failwith "opun fail"

let rec parse_args args =
	match args with
	|ASTArg(arg) -> parse_arg arg
	|ASTArgs(arg,args) -> (parse_arg arg)@(parse_args args)

and parse_arg arg =
		match arg with
		|Arg(id,t) -> id::[]


let rec eval_args env mem args =
	match args with
		|ASTExpr(a) ->  (eval_expr env mem a)::[]
		|ASTExprs(a,b) -> (eval_expr env mem a)::(eval_args env mem b)


and eval_cmds env mem s ast =
	match ast with
	|ASTDec(dec,cmds) -> let (envP,memP) = eval_dec env mem dec in eval_cmds envP memP s cmds
	|ASTStat(stat) -> eval_stat env mem s stat
	|ASTStatcmd(stat,cmds) -> let (memP,sP) = eval_stat env mem s stat in eval_cmds env memP sP cmds


and eval_block env mem s ast =
	match ast with
	|ASTBlock(cmds) -> eval_cmds env mem s cmds

and eval_stat env mem s ast =
	match ast with
	|ASTEcho(e) ->let res = eval_expr env mem e in s:=!s^(get_string res)^"\n";(mem,s)
	|ASTSet(id,e) ->
	(match List.assoc id env with
		InA(a)-> let v = (List.assoc a mem) 
		and affect = eval_expr env mem e in v:= affect;(mem,s)
		|_ -> failwith "SET fail : not in InA")
	 |ASTIfb(e,b1,b2) -> if (eval_expr env mem e) = InN(1) 
	 then (eval_block env mem s b1) 
	 else (eval_block env mem s b2)
	 |ASTWhile(e,b) ->
	 if (eval_expr env mem e) = InN(0)
	 then (mem,s)
	 else let (memP,sP) = (eval_block env mem s b) in
	 	eval_stat env memP sP ast
	|ASTCall(p,args) -> let eval_p = eval_expr env mem p
	and args_list = eval_args env mem args in
	(match eval_p with
		|InP(block,params,env1) ->
		let closure_env = (List.map2(fun x y -> (x,y)) 
		params args_list)@env1 in
		eval_block closure_env mem s block
		|InPR(p,InP(block,params,env1)) ->
		let closure_env = 
		(p,List.assoc p env)::(List.map2 (fun x y -> (x,y)) params args_list)@env1 in
			eval_block closure_env mem s block
		|_ -> failwith "eval_p fail")
							
and eval_dec env mem ast =
	match ast with
	|ASTConst(id,t,e) -> let v = eval_expr env mem e in  ((id,v)::env,mem)
	|ASTFun(id,t,args,e) -> ((id,InF(e,parse_args args,env))::env,mem)
	|ASTFunRec(id,t,args,e) -> 
	let params = parse_args args in((id,InFR(id,InF(e,params,env)))::env,mem)
	(*APS1*)
	|ASTVar(id,t) -> let (a,memP) = alloc(mem) in ((id,InA(a))::env,memP)
	|ASTProc(id,args,b) -> ((id,InP(b,parse_args args,env))::env,mem)
	|ASTProcRec(id,args,b) -> ((id,InPR(id,InP(b,parse_args args,env)))::env,mem)

and eval_expr env mem ast =
	match ast with
	ASTTrue -> InN(1)
	|ASTFalse -> InN(0)
	|ASTNum(n) -> InN(n)
	|ASTId(id) -> 
	(match (List.assoc id env) with
		|InA(a) -> !(List.assoc a mem)
		|v -> v)
	|ASTLambda(args,e) -> InF(e,parse_args args,env)
	|ASTIf(e1,e2,e3) -> if (eval_expr env mem e1) = InN(1) 
	then (eval_expr env mem e2) 
	else (eval_expr env mem e3)
	|ASTApply(e,args) -> 
	let eval_e = eval_expr env mem e  
	and args_list = eval_args env mem args in
	(match eval_e with
		|InF(body,params,env1) -> let closure_env = (List.map2 
		(fun x y -> (x,y)) params args_list)@env1 in
		eval_expr closure_env mem body
		|InFR(f,InF(body,params,env1)) -> let closure_env =
		(f,List.assoc f env)::(List.map2 (fun x y -> (x,y)) 
		params 	args_list)@env1 in
		eval_expr closure_env mem body
		|_ -> failwith "eval_e fail")
	|ASTUPrim(opun,arg) ->
	(match opun with
		|Not -> InN(eval_opun "not" (get_int (eval_expr env mem arg))))	
	|ASTBPrim(opbin,arg1,arg2) ->
	let v1 = get_int (eval_expr env mem arg1) 
	and v2 = get_int (eval_expr env mem arg2) in
	(match opbin with
			| And -> InN(eval_opbin  "and" v1 v2)
			| Or -> InN(eval_opbin "or" v1 v2)
			| Eq ->  InN(eval_opbin  "eq" v1 v2)
			| Lt ->  InN(eval_opbin "lt" v1 v2)
			| Add -> InN(eval_opbin  "add" v1 v2)
			| Sub -> InN(eval_opbin  "sub" v1 v2)
			| Mul -> InN(eval_opbin  "mul" v1 v2)
			| Div -> InN(eval_opbin "div" v1 v2))
			
let eval_prog ast =
	match ast with
	|ASTProg(cmds) -> let output = ref "output :\n"
	and env = []
	and mem = [] in
	let (memory,output) = eval_cmds env mem output cmds in !output

let _ =
	let fic = open_in Sys.argv.(1) in
	let lexbuf = Lexing.from_channel fic in
	let ast = Parser.prog Lexer.token lexbuf in
	let output  = eval_prog ast in
	print_string output;
	print_newline();;			
