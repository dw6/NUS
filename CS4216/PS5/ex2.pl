%%%%%%%%%%%%%%%%%%%%%%%
% Benjamin Tan Wei Hao
% PS 5 
%%%%%%%%%%%%%%%%%%%%%%%
:- lib(ic).
:- lib(ic_global).

solve(L,B) :-
	B1 is B-1,
	length(LR,B),
	LR::0..B1,
	reverse(L,LR),
	% Speeds things up significantly
	sum(L) #= B, 
	(foreach(E,L), count(I,0,B1), param(L) do writeln(L), occurrences(I,L,E)
), labeling(L).


:- solve(L,20), write('Result: '), writeln(L).
:- halt.