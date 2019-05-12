main_stdin :-
	read(user_input,T),
	typeProg([],T,R),
	print(R).
	
/*utilitaires*/
mem(X, [X|_]).
mem(X, [_|XS]) :- mem(X,XS).

assoc(K, [(K,V)|_], V).
assoc(K, [_|KS], V) :- assoc(K, KS, V).

get_type([],[]).
get_type([A|ARGS],[T|TYPES]) :-
	typeExpr([],A,T),
	get_type(ARGS,TYPES).

get_typeArgs([],[]).
get_typeArgs([(_,T)|ARGS],[T|RES]) :-
	get_typeArgs(ARGS,RES).
		
checkArgs(_,[],[]).
checkArgs(C,[ARG|ARGS],[ARGTYPE|ARGSTYPE]) :-
	typeExpr(C,ARG,ARGTYPE),
	checkArgs(C,ARGS,ARGSTYPE).


	
/*prog*/
typeProg(C,prog(X),void) :- typeCmds(C,X,void).

/***APS1***/
/*block*/
typeBlock(C, block(X), void) :- typeCmds(C,X,void).
/******/

/*end(epsilon)*/
typeCmds(_,[epsilon],void).

typeCmds(C,[stat(X)|Y],void) :-
	typeStat(C,X,void),
	typeCmds(C,Y,void).

/*dec*/
typeCmds(C,[dec(X)|Y],void) :-
	typeDec(C,X,CB),
	typeCmds(CB,Y,void).

/*echo*/
typeStat(C,echo(X),void) :-
	typeExpr(C,X,int).	

/***APS1***/
/*set*/
typeStat(C,set(id(ID),EXPR),void) :-
	assoc(ID,C,TYPE),
	typeExpr(C,EXPR,TYPE).

/*ifb*/
typeStat(C,ifb(COND,B1,B2),void) :-
	typeExpr(C,COND,bool),
	typeBlock(C,B1,void),
	typeBlock(C,B2,void).
	
/*while*/
typeStat(C,while(COND,B),void) :-
	typeExpr(C,COND,bool),
	typeBlock(C,B,void).
	
/*call*/
typeStat(C,call(id(P),ARGS),void) :-
	assoc(P,C,arrow(ARGSTYPE,void)),
	checkArgs(C,ARGS,ARGSTYPE).
/******/

/*const*/
typeDec(C,const(X,T,E),[(X,T)|C]) :-
	typeExpr(C,E,T).
	
/*Fun*/
typeDec(C,fun(ID,T,ARGS,BODY),[(ID,arrow(RES,T))|C]):-
	append(ARGS,C,CT),
	typeExpr(CT,BODY,T),
	get_typeArgs(ARGS,RES).
	
/*funRec*/
typeDec(C,funrec(ID,T,ARGS,BODY),[(ID,arrow(RES,T))|C]):-
	get_typeArgs(ARGS,RES),
	append(ARGS,C,CT),
	CTT = [(ID,arrow(RES,T))|CT],
	typeExpr(CTT,BODY,T).

/**************APS1*************/

/*var*/
typeDec(C,var(X,TYPE),CB) :-
	CB=[(X,TYPE)|C].

/*proc*/
typeDec(C,proc(ID,ARGS,BODY),[(ID,arrowtype(RES,void))|C]):-
	append(ARGS,C,CT),
	typeBlock(CT,BODY,void),
	get_typeArgs(ARGS,RES).
	
	
/*procRec*/
typeDec(C,procrec(ID,ARGS,BODY),[(ID,arrowtype(RES,void))|C]):-
	get_typeargs(ARGS,RES),
	append(ARGS,C,CT),
	CTT = [(ID,arrowtype(RES,void))|CT],
	typeBlock(CTT,BODY,void).
	
/******/

/*Expressions*/

/*true*/
typeExpr(_,true,bool).

/*false*/
typeExpr(_,false,bool).

/*num*/
typeExpr(_,num(X),int) :-
 	integer(X).
 	
/*ident*/
typeExpr(C,id(X),T) :-
	assoc(X,C,T).

/*if*/
typeExpr(C,if(COND,E1,E2),T) :-
	typeExpr(C,COND,bool),
	typeExpr(C,E1,T),
	typeExpr(C,E2,T).

/*opérations entières */
typeExpr(C,add(X,Y),int) :-
	typeExpr(C,X,int),
	typeExpr(C,Y,int).

typeExpr(C,sub(X,Y),int) :-
	typeExpr(C,X,int),
	typeExpr(C,Y,int).

typeExpr(C,mul(X,Y),int) :-
	typeExpr(C,X,int),
	typeExpr(C,Y,int).

typeExpr(C,div(X,Y),int) :-
	typeExpr(C,X,int),
	typeExpr(C,Y,int).

/*opérations booléennes*/
typeExpr(C,and(X,Y),bool) :-
	typeExpr(C,X,bool),
	typeExpr(C,Y,bool).

typeExpr(C,or(X,Y),bool) :-
	typeExpr(C,X,bool),
	typeExpr(C,Y,bool).

typeExpr(C,eq(X,Y),bool) :-
	typeExpr(C,X,int),
	typeExpr(C,Y,int).

typeExpr(C,lt(X,Y),bool) :-
	typeExpr(C,X,int),
	typeExpr(C,Y,int).
	
typeExpr(C,not(X),bool) :-
	typeExpr(C,X,bool).
	
/*app*/
typeExpr(C,apply(id(F),ARGS),TF) :-
	assoc(F,C,arrowtype(ARGSTYPE,TF)),
	checkArgs(C,ARGS,ARGSTYPE).
		
typeExpr(C,apply(lambda(ARGSTYPE,BODY),ARGS),TF) :-
	get_typeArgs(ARGSTYPE,RES),
	checkArgs(C,ARGS,RES),
	append(ARGSTYPE,C,CB),
	typeExpr(CB,BODY,TF).
	
typeExpr(C,apply(apply(X,Y),ARGS),TR) :-
	get_type(ARGS,RES),
	typeExpr(C,apply(X,Y),arrowtype(RES,TR)).
				
/*abs*/
typeExpr(C,lambda(ARGS,BODY),arrow(_,TF)) :-
	append(ARGS,C,CB),
	typeExpr(CB,BODY,TF).	
	

   

