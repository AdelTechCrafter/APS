open Ast;;

type valeur = InN of int | InF of expr * string list | InFR of valeur | None
type ident = Pair of string * valeur

let environnement = ref [];;
let output_flux = ref [];;

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
	ASTArgs(a1,a2) -> create_fermeture a2 (env@[get_id a1])
	|ASTArg(a) -> env@[get_id a]

let rec extract_values expr res_values =
	match expr with
	ASTExprs(e,exprs) -> extract_values exprs (res_values@[e])
	| ASTExpr(e) -> res_values@[e]
;;

let rec extract_fun expr =
	match expr with 
	InF(e_prim,fermeture) -> (e_prim,fermeture)
	| InFR(fermeture) -> extract_fun fermeture
	| _ -> failwith "not a function"
;;

let create_env_fun fermeture exprs =
	List.map2 (function a -> function e -> Pair(a,e) ) fermeture exprs
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
	| ASTLambda(args,e_prim) -> InF(e_prim,create_fermeture args [])
	| ASTApply(expr,exprs) -> ( match (eval_expr expr env) with
				InF(e_prim,fermeture) -> let (e_prim,f) = extract_fun (eval_expr expr env) in
				eval_expr e_prim ((create_env_fun f (List.map (function e -> eval_expr e env) (extract_values exprs []) ) )@env)
				 
				| InFR(fermeture) -> let (e_prim,f) =  (extract_fun fermeture) in
					eval_expr (ASTApply(e_prim,exprs)) ((create_env_fun f (List.map (function e -> eval_expr e env) (extract_values exprs []) ) )@env)
				| InN(n) -> InN(n)
				| _ -> failwith "not an appliable function")
;;

let eval_dec dec env=
	match dec with
	| ASTConst(id,t,expr) -> (Pair(id,(eval_expr expr env))::env)
	| ASTFun(id,t,args,expr) -> (Pair(id,InF(expr,create_fermeture args []))::env)
	| ASTFunRec(id,t,args,expr) -> (Pair(id, InFR(InF(expr,create_fermeture args [])))::env)

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
	ASTProg(cmds) -> print_output (eval_cmds cmds !environnement !output_flux)

let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let p = Parser.prog Lexer.token lexbuf in
			(eval_prog p)
	with Lexer.Eof -> exit 0
