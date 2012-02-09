is_term(X * Y) :- number(X), var(Y).
n_term(X * Y, X * Y) :- number(X), !.
n_term(X * Y, Y * X) :- number(Y), !.

n_term(InX+InY, OutX+OutY) :- !,
	n_term(InX, AuxX), n_term(InY, AuxY), n_term(AuxX,OutX), n_term(AuxY,OutY).
n_term(InX-InY, OutX-OutY) :- !,
	n_term(InX, AuxX), n_term(InY, AuxY), n_term(AuxX,OutX), n_term(AuxY,OutY).

eqn(InX+InY=Z, OutX+OutY=Z) :- !, n_term(InX,OutX), n_term(InY,OutY).
eqn(InX-InY=Z, OutX-OutY=Z) :- !, n_term(InX,OutX), n_term(InY,OutY).

% Collect unique variables from each of the equations.
get_vars(InL,OutL) :- !, get_vars(InL, [], AuxL), sort(AuxL,OutL). 
get_vars(N*C,InL,OutL) :- number(N), var(C), append([C],InL, OutL).
get_vars(X+Y,InL,OutL) :- !, get_vars(X,InL,AuxL), get_vars(Y,AuxL,OutL).
get_vars(X-Y,InL,OutL) :- !, get_vars(X,InL,AuxL), get_vars(Y,AuxL,OutL).
get_vars(Terms=_, InL, OutL) :- !, get_vars(Terms,InL,OutL).
get_vars([H|T],InL,OutL) :- !, get_vars(H,InL,AuxL), get_vars(T,AuxL, OutL).
get_vars([],L,L) :- !.




% sort_eqns(InL,[HOut|TOut]) :-
% 	get_vars(InL,V), InL =..[H|T], sort_eqns(H,V,HOut), sort_eqns(T,V,TOut). 

% sort_eqns(Eqn, Out) :- !, get_vars(Eqn,V), sort_eqns(Eqn,V, Out).

% get_terms(Eqn, [VH|VT], Out) :-
% 	!, get_term(Eqn,VH, A), writeln(A), get_terms(Eqn, VT, OutB), 
% 	writeln(OutB),
% 	Out =..[+, A, OutB].
% get_terms(_, [], _) :- !.

get_coeff(Var1,N*Var2,N) :- (Var1 == Var2),!.
get_coeff(Var1,_ + N*Var2,N) :- (Var1 == Var2),!.
get_coeff(Var,Rest + _,N) :- get_coeff(Var,Rest,N),!.
get_coeff(_,_,0).



augrows([E|ET],V,[C|CT]) :- !, augrow(E,V,C), augrows(ET,V,CT). 
augrows([],_,[]) :- !.

augrow(Eqn, [V|VT], [C|CT]) :- !, get_coeff(V,Eqn,C), augrow(Eqn,VT,CT).
augrow(_Eqn, [], []) :- !.	

% solve_lin( [ X*2+Y=3, 3*Y-2*X=1 ] ).









