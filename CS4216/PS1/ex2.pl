% Benjamin Tan Wei Hao
% U077129N
% PS2 - Exercise 2

% Known bugs: Doesn't handle missing variables properly.

solve_lin(Eqns) :-
	get_vars(Eqns,OutV), 
	aug_rows(Eqns,AugMx),
	gauss(AugMx,Mx),
	solver(Mx,OutV), !.

rhs_sum(A,B, X) :- !, zip(A,B,C), sum(C,X).	
zip([A|AT],[B|BT],[C|CT]) :- !, C is A * B, zip(AT,BT,CT).
zip([],[],[]) :- !.

sum([], 0).
sum([H|T],S) :- sum(T,S1), S is H + S1.

% Given an augmented matrix, and a list of variables, solve the system of eqns.
solver(AugM,Vars) :- reverse(AugM,AR), reverse(Vars,VR), solver(AR,VR,[]).
solver([H|T],[VH|VT],PIn) :- solve(H,VH,PIn,POut), solver(T,VT,POut).
solver([],[],_) :- !.

solve(E,V,VIn,VOut) :-
	length(E,2) -> 
		(E = [A,B], V is B/A, append(VIn,[V],VOut));
		(
			E = [EH|ET], reverse(ET,ETR),
			ETR = [Z|ETRR],
			reverse(ETRR,EBody),
			reverse(VIn,VInR),
			rhs_sum(EBody,VInR,RHS),
			V is (Z-RHS)/EH,
			append(VIn,[V],VOut)
		).
solve([],_,_,_) :- !.

gauss([],[]) :- !.
gauss(MatrixIn,[Pivot|MatrixAuxOut]) :- 
	get_pivot(MatrixIn,Pivot,MatrixRest), 
	apply_pivot_matrix(Pivot,MatrixRest,MatrixAux),
	gauss(MatrixAux,MatrixAuxOut).

% Apply pivot to entire matrix.
apply_pivot_matrix(P,[M|MT],[R|RT]) :- !, apply_pivot_row(P,M,R), apply_pivot_matrix(P,MT,RT).
apply_pivot_matrix(_,[],[]) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Apply Pivot row P onto R, giving RO. 
% Assume that P and R have been normalized.

apply_pivot_row(P,R,RO) :- 
	norm_row(P,PP), norm_row(R,RR), 
	length(PP,PL),length(RR,RL), PL > 1, RL > 2,
	apply_pivot_h(PP,RR,[_|RO]), !.

apply_pivot_row(_,[RH|R],R) :- RH =:= 0, !, writeln(R).

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
get_vars(C,InL,OutL) :- var(C), append([C],InL, OutL).
get_vars(N*C,InL,OutL) :- number(N), var(C), append([C],InL, OutL).
get_vars(C*N,InL,OutL) :- number(N), var(C), append([C],InL, OutL).
get_vars(X+Y,InL,OutL) :- !, get_vars(X,InL,AuxL), get_vars(Y,AuxL,OutL).
get_vars(X-Y,InL,OutL) :- !, get_vars(X,InL,AuxL), get_vars(Y,AuxL,OutL).
get_vars(Terms=_, InL, OutL) :- !, get_vars(Terms,InL,OutL).
get_vars([H|T],InL,OutL) :- !, get_vars(H,InL,AuxL), get_vars(T,AuxL, OutL).
get_vars([],L,L) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Retrieve coefficients of the equation given the (sorted) variables list.
% ?- get_coeff(B, -1*X-2*B+3*C, Out) => Out = -2.
get_coeff(V1,V2,1) :- V1 == V2, !.
get_coeff(V1,N*V2,N) :- V1 == V2, !.
get_coeff(V1,V2*N,N) :- V1 == V2, !.
get_coeff(V1,_A+N*V2,N) :- V1 == V2, !.
get_coeff(V1,_A-N*V2,N1) :- V1 == V2, N1 is N * -1, !.
get_coeff(V1,_A+V2,1) :- V1 == V2, !.
get_coeff(V1,_A-V2,-1) :- V1 == V2, !.
get_coeff(V1,_A+V2*N,N) :- V1 == V2, !.
get_coeff(V1,_A-V2*N,N1) :- V1 == V2, N1 is N * -1, !.
get_coeff(Var,A+_B,N) :- get_coeff(Var,A,N),!.
get_coeff(Var,A-_B,N) :- get_coeff(Var,A,N),!.
get_coeff(_,_,0).


% Test Cases
% :- gauss([[2,1,3],[-2,3,1]],X),writeln(X). % (X,Y)=(1,1)
% :- gauss([[3,4,1,6],[2,-1,2,-5],[1,3,-1,9]],X),writeln(X). % (X,Y,Z)=(2,1,-4)
% :- gauss([[2,-3,1,-5],[3,2,-1,7],[1,4,-5,3]],X),writeln(X). % (X,Y,Z)=(1,3,2)
% :- gauss([[1,-1,0,2],[2,-1,-1,3],[1,1,1,6]],X),writeln(X). % (X,Y,Z)=(3,1,2)
% :- gauss([[1,-2,1,0],[2,1,-3,5],[4,-7,1,-1]],X),writeln(X). % (X,Y,Z)=(3,2,1).
% :- gauss([[1,-3,1,4],[2,-8,8,-2],[-6,3,-15,9]],X),writeln(X). % (X,Y,Z)=(3,-1,-2)


% Test Cases 
:- solve_lin([2*X+1*Y=3, 3*Y-2*X=1]), 
   write('X='),writeln(X),write('Y='),writeln(Y),nl.
:- solve_lin([3*X+4*Y+1*Z=6, 2*X-1*Y+2*Z = -5, 1*X+3*Y-1*Z=9]),
   write('X='),writeln(X),write('Y='),writeln(Y),write('Z='),writeln(Z),nl.   % (X,Y,Z)=(2,1,-4)
:- solve_lin([2*X-3*Y+1*Z = -5, 3*X+2*Y-1*Z = 7,1*X+4*Y-5*Z = 3]),
   write('X='),writeln(X),write('Y='),writeln(Y),write('Z='),writeln(Z),nl. % (X,Y,Z)=(1,3,2)
:- solve_lin([1*X-1*Y+0*Z = 2, 2*X-1*Y-1*Z = 3, 1*X+1*Y+1*Z = 6]),
   write('X='),writeln(X),write('Y='),writeln(Y),write('Z='),writeln(Z),nl. % (X,Y,Z)=(3,1,2)
:- solve_lin([1*X-3*Y+1*Z = 4,2*X-8*Y+8*Z = -2,-6*X+3*Y-15*Z = 9]),
   write('X='),writeln(X),write('Y='),writeln(Y),write('Z='),writeln(Z),nl. % (X,Y,Z)=(3,-1,-2)