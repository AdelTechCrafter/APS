/*utilitaires*/
mem(X,[X|_]).
mem(X,[_|Z]):-mem(X,Z).
assoc(K,[(K,V)|_],V).
assoc(K,[_|L],V):-assoc(K,L,V).
append([],L,L).
append([X|Y],L,[X|L2]):-append(Y,L,L2).

typeExprs(_,[],[]).
typeExprs(G,[E|EL],[T|TL]):-typeExpr(G,E,T),typeExprs(G,EL,TL).

getTypes([(_,T)],[T]).
getTypes([(_,T)|ARGS],[T|LT]):-getTypes(ARGS,LT).
	
/*Expressions*/

typeExpr(_,true,bool).
typeExpr(_,false,bool).
typeExpr(_,X,int):-integer(X).
	
typeExpr(G,not(X),bool):-
    typeExpr(G,X,bool).

typeExpr(G,and(X,Y),bool):-
    typeExpr(G,X,bool),
    typeExpr(G,Y,bool).

typeExpr(G,or(X,Y),bool):-
    typeExpr(G,X,bool),
    typeExpr(G,Y,bool).
    
typeExpr(G,eq(X,Y),bool):-
    typeExpr(G,X,int),
    typeExpr(G,Y,int).

typeExpr(G,lt(X,Y),bool):-
    typeExpr(G,X,int),
    typeExpr(G,Y,int).

typeExpr(G,add(X,Y),int):-
    typeExpr(G,X,int),
    typeExpr(G,Y,int).
                
typeExpr(G,sub(X,Y),int):-
    typeExpr(G,X,int),
    typeExpr(G,Y,int).

typeExpr(G,mul(X,Y),int):-
    typeExpr(G,X,int),
    typeExpr(G,Y,int).


typeExpr(G,div(X,Y),int):-
    typeExpr(G,X,int),
    typeExpr(G,Y,int).

/*IF*/
typeExpr(G,if(COND,CONS,ALTER),T):-
	typeExpr(G,COND,bool),
	typeExpr(G,CONS,T),
	typeExpr(G,ALTER,T).

/*Symbole*/
typeExpr(G,X,T):-mem((X,T),G). 

/*ABSTRACTION*/
typeExpr(G,functionAnonyme(ARGS,E),typefun(TS,T)):-
	getTypes(ARGS,TS),
	append(G,ARGS,GG),
	typeExpr(GG,E,T).

/*Application(appel de fonction)*/
typeExpr(G,application(F,LE),T):-
	typeExpr(G,F,typefun(LT,T)),
	typeExprs(G,LE,LT).
	/*LT : type des arguments*/
	/*T:type de retour de la fonction*/

/*Echo*/
typeState(G,echo(X),void):-
    typeExpr(G,X,int).

/*Declaration*/

/*declaration const*/
typeDec(G,const(I,T,E),[(I,T)|G]):-
    typeExpr(G,E,T).
  

/*declaration de fonction*/
typeDec(G,fun(X,T,ARGS,E),[(X,typefun(TS,T))|G]):-
	append(G,ARGS,G2),
	typeExpr(G2,E,T),
	getTypes(ARGS,TS).

/*declaration de fonctions récursive*/
typeDec(G,funRec(X,T,ARGS,E),[(X,typefun(TS,T))|G]):-
	append(G,ARGS,G2),
	append(G2,[(X,typefun(TS,T))],G3),
	typeExpr(G3,E,T),
	getTypes(ARGS,TS).
	
	
/*sequences(suite de commandes)*/

/*commence par une déclaration*/
typeCmds(G,[DEC|CMDS],void):-
	typeDec(G,DEC,G2),
	typeCmds(G2,CMDS,void).
/*commence par une instruction*/
typeCmds(G,[STAT|CMDS],void):-
	typeState(G,STAT,void),
	typeCmds(G,CMDS,void).

/*suite vide*/
typeCmds(_,[epsilon],void).

/*Programmes*/
typeProg(prog(CMDS),void):-
	typeCmds([],CMDS,void).

main(File) :-
	open(File,read,Str),
	read(Str,Prog),
	close(Str),
	typeProg(Prog,void), nl.

	





    
   

