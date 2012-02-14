% Benjamin Tan Wei Hao
% Problem Set 2 - Exercise 2
:- dynamic qs/2.
:- dynamic part/4.
:- dynamic app/3.

% example used.
qs([],[]).
qs([X|Xs],Ys) :- 
	part(X,Xs,Littles,Bigs),
	qs(Littles,Ls),
	qs(Bigs,Bs),
	app(Ls,[X|Bs],Ys).
part(_,[],[],[]).
part(X,[Y|Xs],[Y|Ls],Bs) :- X > Y, part(X,Xs,Ls,Bs).
part(X,[Y|Xs],Ls,[Y|Bs]) :- X =< Y, part(X,Xs,Ls,Bs).

app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).


solve(P) :-
	setval(count,0),
	(solve(P,_);true), % forces this to be true ...
	getval(count,N),   % so that we can print the final count.
	writeln(N).

solve(true,0) :-  !.
solve(A,1) :- arithmetic(A), !, A.
solve((A,B),N) :- !, solve(A,N1), solve(B,N2), N is N1 + N2, setval(count,N).
solve(H,N) :- clause(H,Body), solve(Body,M), N is M+1, incval(count).

arithmetic(_<_).
arithmetic(_=<_).
arithmetic(_=:=_).
arithmetic(_=\=_).
arithmetic(_>=_).
arithmetic(_>_).

% solve(qs([9,8,7,6,5,4,3,2,1],X),N).
% solve(qs([9,8,7,6,5,4,3,2,1],[]),N).