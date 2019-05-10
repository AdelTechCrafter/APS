open Ast;;

type valeur = InN of int | InF | InFR | None
type ident = Pair of string * valeur

let env = ref [];;

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
	| _ -> failwith "not implemented yet"

let copy_env =
	let copy = ref [] in
		(let rec duplicate liste =
			match liste with
			value::tl -> copy := [value]@(!copy)
			| [] -> ()
		in (duplicate !env; !copy) )
		
let rec eval e =
	match e with
	ASTNum n -> InN(n)
	| ASTBool b -> if b then InN(1)  else InN(0)
	| ASTId id -> if (mem_env id !env) then extract_from_env id !env else failwith "not a variable"
	| ASTPrim(op,e1,e2) -> if is_op op then  (eval_op op (eval e1) (eval e2) ) else failwith "not an operator"
	| ASTIf(e1,e2,e3) -> if (eval e1) = InN(1) then eval e2 else eval e3
	| _ -> None

let _=
	try
		let lexbuf = Lexing.from_channel stdin in
		let e =  Parser.line Lexer.token lexbuf in
		print_val (eval e);
	with Lexer.Eof -> exit 0
