is_term(X * Y) :- number(X), var(Y).
n_term(X * Y, X * Y) :- number(X), !.
n_term(X * Y, Y * X) :- number(Y), !.

n_term(InX+InY, OutX+OutY) :- !,
	n_term(InX, AuxX), n_term(InY, AuxY), n_term(AuxX,OutX), n_term(AuxY,OutY).
n_term(InX-InY, OutX-OutY) :- !,
	n_term(InX, AuxX), n_term(InY, AuxY), n_term(AuxX,OutX), n_term(AuxY,OutY).

eqn(InX+InY=Z, OutX+OutY=Z) :- !, n_term(InX,OutX), n_term(InY,OutY).
eqn(InX-InY=Z, OutX-OutY=Z) :- !, n_term(InX,OutX), n_term(InY,OutY).





%%%%
% Creates an augmented matrix based on the equations.
%
% ?- aug_rows([-1*X-2*B+3*C-4*D-5*R, -4*A-6*R-6*T+5*B], Out).
%    Out = [[-1, -2, 3, -4, -5, 0, 0], [0, 5, 0, 0, -6, -4, -6]].
aug_rows(Eqn,AugMatrix) :- !, get_vars(Eqn, V), aug_rows(Eqn,V,AugMatrix).
aug_rows([E|ET],V,[C|CT]) :- !, aug_row(E,V,C), aug_rows(ET,V,CT). 
aug_rows([],_,[]) :- !.

aug_row(Eqn,[V|VT],[C|CT]) :- !, get_coeff(V,Eqn,C), aug_row(Eqn,VT,CT).
aug_row(_,[],[]) :- !.

% Collect unique variables from each of the equations.
get_vars(InL,OutL) :- !, get_vars(InL, [], AuxL), sort(AuxL,OutL). 
get_vars(N*C,InL,OutL) :- number(N), var(C), append([C],InL, OutL).
get_vars(X+Y,InL,OutL) :- !, get_vars(X,InL,AuxL), get_vars(Y,AuxL,OutL).
get_vars(X-Y,InL,OutL) :- !, get_vars(X,InL,AuxL), get_vars(Y,AuxL,OutL).
get_vars(Terms=_, InL, OutL) :- !, get_vars(Terms,InL,OutL).
get_vars([H|T],InL,OutL) :- !, get_vars(H,InL,AuxL), get_vars(T,AuxL, OutL).
get_vars([],L,L) :- !.

%%%%
% Retrieve coefficients of the equation given the (sorted) variables list.
% 
% ?- get_coeff(B, -1*X-2*B+3*C, Out).
% Out = -2.
get_coeff(V1,N*V2,N) :- V1 == V2, !.
get_coeff(V1,_A+N*V2,N) :- V1 == V2, !.
get_coeff(V1,_A-N*V2,N1) :- V1 == V2, N1 is N * -1, !.
get_coeff(Var,A+_B,N) :- get_coeff(Var,A,N),!.
get_coeff(Var,A-_B,N) :- get_coeff(Var,A,N),!.
get_coeff(_,_,0).


	

% solve_lin( [ X*2+Y=3, 3*Y-2*X=1 ] ).









