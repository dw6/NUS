% Formats input arguments into a list.
to_list({X},L) :- !, to_list(X,L).
to_list((X,Y),[X|T]) :- !, to_list(Y,T).
to_list(X,[X]) :- !.

% Split domains into its Variables, and its allowable Values
% dom_ran([X:3..10,Y:4..10], V, L).
% V = [X, Y]
% L = [[3, 4, 5, 6, 7, 8, 9, 10], [4, 5, 6, 7, 8, 9, 10]]
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

propagate(LC,DC,_NewDoms) :-
	to_list(LC,Cons),
	to_list(DC,DCL), dom_ran(DCL,Vars,Doms),
	solve_all(Vars,Doms,Cons,Solns),
	writeln(Solns).






























% all_solns(Variables,Domains,Constraints,A) :- 
% 	length(Domains,N),length(A,N),
% 	(
% 		fromto(	(Variables,Domains,A), ([X|RestV],[D|RestD],[E|RestA]), (RestV,RestD,RestA), ([],[],[])) do member(E,D), X is E
% 	),(
% 		foreach(Con,Constraints) do Con
% 	).









% Retrieve vars, with duplicates removed.
% 	get_vars({1*X+1*Y=:=4*Z, 1*Y > 1*X}, [] , V).
% 	V = [X, Y, Z]
get_vars({X,Y},VarsIn,VarsOut) :- !,
	vars(X,Vars), append(VarsIn,Vars,VarsAux),
	get_vars({Y},VarsAux,VarsOut1),
	sort(VarsOut1,VarsOut).
get_vars({X},VarsIn,VarsOut) :- !,
	vars(X,Vars), append(VarsIn,Vars,VarsOut).
get_vars({},V,V) :- !.

vars(Term,[]) :- atomic(Term), !.
vars(Term,[Term]) :- var(Term), !.
vars(Term,List) :- compound(Term), functor(Term,_,K), args(K,Term,List).

args(0,_,[]) :- !.
args(K,Term,List) :-
    K > 0, K1 is K-1,
    args(K1,Term,L1s),
    arg(K,Term,A),
    vars(A,L2s), append(L1s,L2s,List).


% Extract Domains (to use with multifor later on)
% 	domains({X:1..4, Y:2..5, Z:1..1}, [], Starts, [], Ends).
% 		Starts = [1, 2, 1]
% 		Ends = [4, 5, 1]
domains({X,Y}, StartIn,StartOut,EndIn,EndOut) :- !,
	domains(X,StartIn,StartAux,EndIn,EndAux),
	domains({Y},StartAux,StartOut,EndAux,EndOut).
domains({X:S..E},StartIn,StartOut,EndIn,EndOut) :- !,
	domains(X:S..E,StartIn,StartOut,EndIn,EndOut).
domains(_X:S..E,StartIn,StartOut,EndIn,EndOut) :-
	append(StartIn, [S], StartOut), append(EndIn, [E], EndOut).

% Tests if X,Y is true or false.
% X is an substituted arithmetic expression.
test((X,Y),Bool) :- 
	test(X,Bool1), test((Y),Bool2), !, 
	((Bool1,Bool2) -> Bool = true ; Bool = false). 
test(X,Bool) :- X -> Bool = true ; Bool = false.


% LC : Linear Constraints
% DC : Domain Constraints
% ND : New Domain
% propagate({LC},{DC},_ND) :-
% 	get_vars({LC}, [] , Vars),
% 	labeling(Vars),
% 	domains({DC}, [], Starts, [], Ends),

% 	(multifor(Vars,Starts,Ends),param(LC,Vars)
% 		do (
			
% 		)
% 	)


% TODO: SORT DOMAINS!

% propagate( { 1*X+1*Y=:=4*Z, 1*Y > 1*X }, {X:1..4, Y:2..5, Z:1..1}, NewDomains).
% propagate( { 1*X < 1*Y }, {X:1..4, Y:2..5}, NewDomains).












