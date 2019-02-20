typeExpr(_,A,int):-
    integer(A).

typeExpr(_,true,bool).
typeExpr(_,false,bool).
	
typeExpr(_,add(A,B),int):-
    typeExpr(_,A,int),
    typeExpr(_,B,int).

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

fetch(X, [(X,V)|_], V).
fetch(X, [_|XS], V) :- fetch(X, XS, V).

member(A,XS).
member(X,[X|_]).
member(X,[Y|XS]):-member(X,XS).

typeDec(G,declarationConst(I,T,E),[(I,T)|G]):-
    typeExpr(_,E,T),
    not(member((I,T),G)),
    append([(I,T)],G,G),
    fetch(I,G,T).

typeDec(G,declaration(I,T,E),[(I,T)|G]):-
    typeExpr(_,E,T),
    not(member((I,T))),
    fetch(I,G,T).
   

