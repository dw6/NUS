

gauss([],[]) :- !.

gauss(AugMx,AugMx) :- length(AugMx, 1), !.

gauss(MatrixIn,[Pivot|MatrixAuxOut]) :- !,
	get_pivot(MatrixIn,Pivot,MatrixRest), 
	apply_pivot_matrix(Pivot,MatrixRest,MatrixAux), writeln(MatrixAux),
	gauss(MatrixAux,MatrixAuxOut).


% Apply pivot to entire matrix.
apply_pivot_matrix(P,[M|MT],[R|RT]) :- !,
	apply_pivot_row(P,M,R),
	apply_pivot_matrix(P,MT,RT).
apply_pivot_matrix(_,[],[]) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Apply Pivot row P onto R, giving RO. 
% Assume that P and R have been normalized.
apply_pivot_row(P,R,RO) :- 
	norm_row(P,PP), norm_row(R,RR), 
	apply_pivot_h(PP,RR,[_|RO]).

% apply_pivot_h([1,2,3], [1,-2,-3], X).
% X = [0, 4, 6].
apply_pivot_h(_,RIn,ROut) :- RIn = [Z|_], Z =:= 0, ROut = RIn.
apply_pivot_h([PH|PT],[H|T],[RH|RT]) :- RH is PH-H, apply_pivot_h(PT,T,RT), !.
apply_pivot_h(_,[],[]) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Returns the first leading non zero in the matrix.
% get_pivot([[0,0,0,1], [4,0,0,3], [2,3,4,5]], X).
% X = [4, 0, 0, 3].
get_pivot(M,P,Rest) :- get_pivot_h(M,P), subtract(M, [P], Rest).
get_pivot_h([R|RT],Pivot) :- R = [Z|_], Z =:= 0, get_pivot_h(RT, Pivot), !.
get_pivot_h([R|_],R):- !.
get_pivot_h([],_):- !.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Normalize row.
% norm_row([-2, 3, -4], X) => X = [1.0, -1.5, 2.0].
norm_row(R,PR) :- R = [Z|T], Z =:= 0, norm_row(T,PR), !.
norm_row(R,PR) :- R = [H|_], H2 is 1/H, norm_row(H2,R,PR), !.
norm_row(F,[H|T],[P|PT]) :- !, P is H*F, norm_row(F,T,PT).
norm_row(_,[],[]) :- !. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Creates an augmented matrix based on the equations.
% aug_rows([-1*X-2*B+3*C-4*D-5*R = 10, -4*A-6*R-6*T+5*B = 20], Out).
% Out = [[-1, -2, 3, -4, -5, 0, 0, 10], [0, 5, 0, 0, -6, -4, -6, 20]].
aug_rows(Eqn,AugMatrix) :- !, get_vars(Eqn, V), aug_rows(Eqn,V,AugMatrix).

% Appends the = X part to result coefficient list.
aug_rows([E=Z|ET],V,[C1|CT]) :- 
	!, aug_row(E,V,C),append(C,[Z],C1), aug_rows(ET,V,CT). 
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Retrieve coefficients of the equation given the (sorted) variables list.
% ?- get_coeff(B, -1*X-2*B+3*C, Out) => Out = -2.
get_coeff(V1,N*V2,N) :- V1 == V2, !.
get_coeff(V1,_A+N*V2,N) :- V1 == V2, !.
get_coeff(V1,_A-N*V2,N1) :- V1 == V2, N1 is N * -1, !.
get_coeff(Var,A+_B,N) :- get_coeff(Var,A,N),!.
get_coeff(Var,A-_B,N) :- get_coeff(Var,A,N),!.
get_coeff(_,_,0).

% solve_lin( [ X*2+Y=3, 3*Y-2*X=1 ] ).




:- gauss([[2,1,3],[-2,3,1]], X), writeln(X).




