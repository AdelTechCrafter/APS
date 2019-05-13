open Ast;;

type valeur = InN of int | InF of expr * string list * ident list | InFR of valeur | None
and ident = Pair of string * valeur

let print_val value =
	match value with
	InN(n) -> Printf.printf "%d\n" n
	| _ -> failwith "not a printable value"

let rec mem_env id liste =
	match liste with
	Pair(i,v)::tl -> if (String.equal id i) then true else mem_env id tl
	| [] -> false
;;

let rec extract_from_env id liste =
	match liste with
	Pair(i,v)::tl -> if (String.equal id i) then v else extract_from_env id tl
	| _ -> failwith "erreur extraction variable"
;;

let is_opbin op =
	match op with
	 Add -> true
	| Mul -> true
	| Sub -> true
	| Div -> true
	| And -> true
	| Or -> true
	| Eq -> true
	| Lt -> true

let is_opun op =
	match op with
	Not -> true

let get_int value =
	match value with
	|InN(n) -> n
	| _ -> failwith "not in N"

let eval_opbin opbin e1 e2 =
	match opbin with
	 Add -> InN((get_int e1) + (get_int e2))
	| Mul -> InN((get_int e1) * (get_int e2))
	| Sub -> InN((get_int e1) - (get_int e2))
	| Div -> InN((get_int e1) / (get_int e2))
	| And -> if (((get_int e1) + (get_int e2)) > 2 || (abs ( (get_int e1) - (get_int e2) )) < 0) then failwith "not logical arguments"
		else if (get_int e1) = 0 then InN(0) else InN((get_int e2))
	| Or -> if (((get_int e1) + (get_int e2)) > 2 || (abs ( (get_int e1) - (get_int e2) )) < 0) then failwith "not logical arguments"
		else if (get_int e1) = 1 then InN(1) else InN((get_int e2))
	| Eq -> if (get_int e1) = (get_int e2) then InN(1) else InN(0)
	| Lt -> if (get_int e1) < (get_int e2) then InN(1) else InN(0)

let eval_opun op e =
	match op with
	Not -> if (get_int e) = 0 then InN(1) else if (get_int e) = 1 then InN(0) else failwith "not logical unary argument"

let get_id arg =
	match arg with
	Arg(id,t) -> id
;;

let rec create_fermeture args env =
	match args with
	ASTArgs(a1,a2) -> create_fermeture a2 ([get_id a1]@env)
	|ASTArg(a) -> [get_id a]@env
;;

let rec eval_expr expr env = 
	match expr with
	ASTTrue -> InN(1)
	| ASTFalse -> InN(0)
	| ASTNum(n) -> InN(n)
	| ASTId(id) -> if mem_env id env then extract_from_env id env else failwith (id^" not a variable in environment")
	| ASTBPrim(opbin,e1,e2) -> if is_opbin opbin then eval_opbin opbin (eval_expr e1 env) (eval_expr e2 env) else failwith "not a binary operator"
	| ASTUPrim(opu,e) -> if is_opun opu then eval_opun opu (eval_expr e env) else failwith "not a unary operator"
	| ASTIf(e1,e2,e3) -> if (eval_expr e1 env) = InN(1) then eval_expr e2 env else if (eval_expr e1 env) = InN(0) then (eval_expr e3 env) else failwith "not a boolean value"
	| ASTLambda(args,e_prim) -> InF(e_prim,create_fermeture args [],env)
	
	| ASTApply (expr,exprs) -> match (eval_expr expr env) with
					InF(e_prim,fermeture,envi) -> let env_fun = (assoc_val fermeture (get_eval exprs env))@envi in
									eval_expr e_prim env_fun
					| InFR(fermeture) ->( match fermeture with
								InF(e_prim,f,envi) -> let env_fun = (assoc_val f (get_eval exprs env))@env in
									eval_expr (ASTApply(e_prim,exprs)) env_fun
								| _ -> failwith "not a recursive function")
					| InN(n) -> InN(n)
					| _ -> failwith "application result not applied yet"
and get_eval exprs env =
	match exprs with
	ASTExprs(e,es) -> (eval_expr e env)::(get_eval es env)
	| ASTExpr(e) -> (eval_expr e env)::[]

and assoc_val fermeture exprs =
	List.map2 (function a -> function e -> Pair(a,e) ) fermeture exprs

let eval_dec dec env=
	match dec with
	| ASTConst(id,t,expr) -> (Pair(id,(eval_expr expr env))::env)
	| ASTFun(id,t,args,expr) -> (Pair(id,InF(expr,create_fermeture args [],env))::env)
	| ASTFunRec(id,t,args,expr) -> (Pair(id, InFR(InF(expr,create_fermeture args [],env)))::env)

let eval_stat stat env output =
	match stat with
	| ASTEcho(n) -> (eval_expr n env)::output

let rec eval_cmds cmds env output =
	match cmds with
	ASTDec(dec,c) -> eval_cmds c (eval_dec dec env) output
	| ASTStatcmd(stat,c) -> eval_cmds c env (eval_stat stat env output)
	| ASTStat(stat) -> eval_stat stat env output

let rec print_output sortie =
	List.iter (function x -> print_val x) (List.rev sortie) 
;;

let eval_prog prog =
	match prog with
	ASTProg(cmds) -> print_output (eval_cmds cmds [] [])

let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let p = Parser.prog Lexer.token lexbuf in
			(eval_prog p)
	with Lexer.Eof -> exit 0
