%%%%%%%%%%%%%%%%%%%%%%%
% Benjamin Tan Wei Hao
% PS 5 
%%%%%%%%%%%%%%%%%%%%%%%
:- lib(ic).

solve(L,B) :-
	B1 is B-1,
	length(LR,B),
	LR::0..B1,
	reverse(L,LR),
	% Speeds things up significantly
	sum(L) #= B, 
	(foreach(E,L), count(I,0,B1), param(L) do writeln(L), occ(L,I,E)).

% Constraint version of occurs.
occ([],_,N) :- N is 0.
occ([H|T],X,N) :- occ(T,X,N1), H #= X,  N #= N1+1.
occ([H|T],X,N) :- occ(T,X,N1), H #\= X, N #= N1.

:- solve(L,4), write('Result: '), writeln(L).
:- halt.