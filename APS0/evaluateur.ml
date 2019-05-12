open Ast;;

type valeur = InN of int | InF of expr * string list | InFR of valeur | None
type ident = Pair of string * valeur

let environnement = ref [];;
let output_flux = ref [];;


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


let is_op op =
	match op with
	 Add -> true
	| Mul -> true
	| Sub -> true
	| Div -> true
	| And -> true
	| Or -> true
	| Eq -> true
	| Lt -> true
	| Not -> true

let get_int value =
	match value with
	|InN(n) -> n
	| _ -> failwith "not in N"

let eval_op op e1 e2 =
	match op with
	 Add -> InN((get_int e1) + (get_int e2))
	| Mul -> InN((get_int e1) * (get_int e2))
	| Sub -> InN((get_int e1) - (get_int e2))
	| Div -> InN((get_int e1) / (get_int e2))
	| And -> if (get_int e1) = 0 then InN(0) else InN((get_int e2))
	| Or -> if (get_int e1) = 1 then InN(1) else InN((get_int e2))
	| Eq -> if (get_int e1) = (get_int e2) then InN(1) else InN(0)
	| Lt -> if (get_int e1) < (get_int e2) then InN(1) else InN(0)
	| _ -> failwith "not a binary operator"

let eval_unary_op op e =
	match op with
	Not -> if get_int e = 1 then InN(0) else InN(1)
	| _ -> failwith "not an unary operator"

let print_val value =
	match value with
	InN(n) -> Printf.printf "InN(%d)\n" n
	| _ -> failwith "not a printable value"

let copy_env env=
	let copy = ref [] in
		(let rec duplicate liste =
			match liste with
			value::tl -> copy := [value]@(!copy)
			| [] -> ()
		in (duplicate env; !copy) )

let rec get_id arg =
	match arg with
	ASTArg(id,t) -> get_id id
	|ASTId(id) -> id
	| _ -> failwith "cannot extract an identifier"
;;

let rec create_fermeture args env =
	match args with
	ASTArgs(a1,an) -> create_fermeture an (env@[(get_id a1)])
	| ASTArg(id,t) -> (env@[(get_id id)])
	| _ -> failwith "not a list of arguments"

let rec extract_values expr res_values =
	match expr with
	ASTSequence(e,exprs) -> extract_values exprs (res_values@[e])
	| ASTSingle(e) -> res_values@[e]
	|_ -> failwith "not a sequence of expressions"
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
		
let rec eval e env=
	match e with
	ASTNum n -> InN(n)
	| ASTBool b -> if b then InN(1)  else InN(0)
	| ASTId id -> if (mem_env id env) then extract_from_env id env else failwith (id^" not a variable")
	| ASTPrim(op,e1,e2) -> if is_op op then  (eval_op op (eval e1 env) (eval e2 env) ) else failwith "not an operator"
	| ASTIf(e1,e2,e3) -> if (eval e1 env) = InN(1) then eval e2 env else if (eval e1 env) = InN(0) then (eval e3 env) else failwith "not a boolean value"
	| ASTAbs(args,e_prim) -> InF(e_prim,create_fermeture args [])
	| ASTApp(expr,args_concrets) ->( match (eval expr env) with
				InF(e_prim,fermeture) -> let (e_prim,f) = extract_fun (eval expr env) in
				eval e_prim ((create_env_fun f (List.map (function e -> eval e env) (extract_values args_concrets []) ) )@env)
				 
				| InFR(fermeture) -> let (e_prim,f) =  (extract_fun fermeture) in
					eval (ASTApp(e_prim,args_concrets)) ((create_env_fun f (List.map (function e -> eval e env) (extract_values args_concrets []) ) )@env)
				| InN(n) -> InN(n)
				| _ -> failwith "not an appliable function")
	| _ -> None

let eval_dec dec env=
	match dec with
	| ASTConst(id,t,expr) -> (Pair(id,(eval expr env))::env)
	| ASTFun(id,t,args,expr) -> (Pair(id,InF(expr,create_fermeture args []))::env)
	| ASTFunRec(id,t,args,expr) -> (Pair(id, InFR(InF(expr,create_fermeture args [])))::env)
	| _ -> failwith "not other dec implemented"

let eval_stat stat env output =
	match stat with
	| ASTEcho(n) -> (eval n env)::output
	| _ -> failwith "not a statement"

let rec eval_cmds cmds env output =
	match cmds with
	ASTDecs(dec,c) -> eval_cmds c (eval_dec dec env) output
	| ASTStats(stat,c) -> eval_cmds c env (eval_stat stat env output)
	| ASTEcho(n) -> (eval n env)::output
	| _ -> failwith "cmds not implemented yet"

let rec print_output sortie =
	match sortie with
	s1::s -> print_val s1; print_output s
	| [] -> ()
;;

let eval_prog prog =
	match prog with
	ASTProg(cmds) -> print_output (eval_cmds cmds !environnement !output_flux)
	| _ -> failwith "not a program"


;;

let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let e = Parser.prog Lexer.token lexbuf in
			eval_prog e
	with Lexer.Eof -> exit 0