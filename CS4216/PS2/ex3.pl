:- lib(matrix_util).

% Formats input arguments into a list.
to_list({X},L) :- !, to_list(X,L).
to_list((X,Y),[X|T]) :- !, to_list(Y,T).
to_list(X,[X]) :- !.

% Split domains into its Variables, and its allowable Values
% dom_ran([X:3..10,Y:4..10], V, L).
% V = [X, Y], L = [[3, 4, 5, 6, 7, 8, 9, 10], [4, 5, 6, 7, 8, 9, 10]]
dom_ran([V:Low..High|Rest], [V|VR], [L|LR]) :- 
	(foreach(E,L), count(I,Low,High) do E = I),
	dom_ran(Rest,VR,LR).
dom_ran([],[],[]) :- !.


solve(Vars,Doms,Cons,Solns) :-
	length(Vars,Len),length(Solns,Len),
	(fromto((Vars,Doms,Solns),([V|VR],[D|DR],[S|SR]),(VR,DR,SR),([],[],[])) do ( member(S,D), V is S)),
	(foreach(C,Cons) do C).

% solve_all([X,Y],[[1,2,3,4],[1,2,3,4]],[1*X + 2*Y =:= 6],S).
solve_all(Vars,Doms,Cons,Solns) :- findall(Soln, solve(Vars,Doms,Cons,Soln),Solns).


% propagate( { 1*X+1*Y=:=4*Z, 1*Y > 1*X }, {X:1..4, Y:2..5, Z:1..1}, NewDomains).
propagate(LC,DC,NDoms) :-
	to_list(LC,Cons), to_list(DC,DCL), dom_ran(DCL,Vars,Doms),
	solve_all(Vars,Doms,Cons,Solns),
	transpose(Solns,TSolns),
	(fromto((Vars,TSolns,NDoms),([V|VT],[D|DT],[{V::Low..High}|NDT]),(VT,DT,NDT),([],[],[]))
	do ( min(D,Low), max(D,High) )).	


























