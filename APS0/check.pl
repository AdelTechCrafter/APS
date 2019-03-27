use_module(library(lists)).
mem(X, [X|_]).
mem(X, [_|XS]) :- mem(X,XS).

assoc(X, [(X,V)|_], V).
assoc(X, [_|XS], V) :- assoc(X, XS, V).

delete_one(X,[X|Z],Z).
delete_one(X,[V|Z],[V|Y]):-
	X\==V,
	delete_one(X,Z,Y).
	
typeExpr(_,A,int):-
    TYPENUM(A).
 
typeExpr(_,true,bool).
typeExpr(_,false,bool).

/*
typeExpr(_,A,int):-
    integer(A).
 
typeExpr(_,true,bool).
typeExpr(_,false,bool).
*/
    
typeExpr(G,A,T):-
	assoc(A,G,T).
	    
typeExpr(G,add(A,B),int):-
    member((A,int),G),
    member((B,int),G).
    /*  typeExpr(_,A,int),
    typeExpr(_,B,int).
    (member((A,int),G),
    typeExpr(_,B,int));
    (member((B,int),G),
    typeExpr(_,A,int));
    (member((A,int),G),
    member((A,int),G)).
    */
    
    
    

	    
typeExpr(_,sub(A,B),int):-
    typeExpr(_,A,int),
    typeExpr(_,B,int).

typeExpr(_,mul(A,B),int):-
    typeExpr(_,A,int),
    typeExpr(_,B,int).


typeExpr(_,div(A,B),int):-
    typeExpr(_,A,int),
    typeExpr(_,B,int).

typeExpr(_,not(A),bool):-
    typeExpr(_,A,bool).

typeExpr(_,and(A,B),bool):-
    typeExpr(_,A,bool),
    typeExpr(_,B,bool).

typeExpr(_,or(A,B),bool):-
    typeExpr(_,A,bool),
    typeExpr(_,B,bool).

typeExpr(_,eq(A,B),bool):-
    typeExpr(_,A,bool),
    typeExpr(_,B,bool).

typeExpr(_,lt(A,B),bool):-
    typeExpr(_,A,int),
    typeExpr(_,B,int).

typeState(_,echo(A),void):-
    typeExpr(_,A,int).

typeExpr(_,if(COND,CONS,ALTER),T):-
	typeExpr(_,COND,bool),
	typeExpr(_,CONS,T),
	typeExpr(_,ALTER,T).
	

fetch(X, [(X,V)|_], V).
fetch(X, [_|XS], V) :- fetch(X, XS, V).


typeDec(G,declarationConst(I,T,E),[(I,T)|G]):-
    typeExpr(_,E,T).
  
 

typeDec(G,declaration(I,T,E),[(I,T)|G]):-
    typeExpr(_,E,T).


    
   

