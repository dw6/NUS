% Benjamin Tan Wei Hao
% U077129N

:- lib(ic).

crypto(Equation) :-
	term_variables(Equation,Vars),
	Vars::0..9,
	alldifferent(Vars), 
	expr(Equation,_),
	labeling(Vars).

expr(E1+E2,ConE1+ConE2) :-
	expr(E1,ConE1),
	expr(E2,ConE2).

expr(E=R,ConE=ConR) :-
	expr(E,ConE),
	expr(R,ConR),
	eval(ConE) #= eval(ConR).

expr(Expr,sum(CExpr)) :-
	length(Expr,LenE),
	Expr = [EH|_], EH $\= 0,	
	(foreach(E,Expr),foreach((10^(LenE-I)*E),CExpr),count(I,1,LenE),param(LenE) do 
		( true )
	).

% :- crypto([G,E,R,A,L,D]+[D,O,N,A,L,D]=[R,O,B,E,R,T]).
% :- crypto([S,E,N,D]+[M,O,R,E]=[M,O,N,E,Y]).
% :- crypto([J,A,V,A]+[L,I,S,P]+[P,A,S,C,A,L]=[S,I,M,U,L,A]). 
% :- crypto([C,P,P]+[L,L,P]+[R,U,B,Y]=[L,O,G,I,C]). 