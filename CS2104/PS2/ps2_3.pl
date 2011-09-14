% Benjamin Tan Wei Hao
% U077129N

% Sample Run:
% sorted((1+0)*(a-x)+(x+2)*(0-1), R), simplify(R,R1), clean(R1, R2).

/**
 sort moves all the numbers to the back, followed by characters in alphateical order
**/

% Multiplication
sorted(0*_, R) :- sorted(0, R), !.
sorted(_*0, R) :- sorted(0, R), !.
sorted(1*Y, R) :- sorted(Y, R), !.
sorted(X*1, R) :- sorted(X, R), !.

sorted(X*Y, R) :- number(X), number(Y), R is X*Y, !.
sorted(X*Y, X*Y) :- atom(X), number(Y), !.
sorted(X*Y, Y*X) :- number(X), atom(Y), !.
sorted(X*Y, Y*X) :- atom(X), atom(Y), char_code(X,Xc), char_code(Y,Yc), Xc > Yc, !.
sorted(X*Y, X*Y) :- atom(X), atom(Y), !.


% Addition
sorted(X+Y, R) :- number(X), number(Y), R is X+Y, !.
sorted(X+Y, X+Y) :- atom(X), number(Y), !.
sorted(X+Y, Y+X) :- number(X), atom(Y), !.
sorted(X+Y, Y+X) :- atom(X), atom(Y), char_code(X,Xc), char_code(Y,Yc), Xc > Yc, !.
sorted(X+Y, X+Y) :- atom(X), atom(Y), !. 

% Subtraction
sorted(X-Y, R) :- number(X), number(Y), R is X-Y, !.
sorted(X-Y, X-Y) :- atom(X), number(Y), !.
sorted(X-Y, -Y+X) :- number(X), atom(Y), !.
sorted(X-Y, -Y+X) :- atom(X), atom(Y), char_code(X,Xc), char_code(Y,Yc), Xc > Yc, !.
sorted(X-Y, X-Y) :- atom(X), atom(Y), !.

% Multiplication
sorted(C*X, R) :- sorted(C, R1), number(R1), number(X), R is R1*X, !. % ok
sorted(C*X, R) :- sorted(C, R1), number(R1), atom(X), R = X*R1, !.% ok
sorted(C*X, R) :- sorted(C, A*B), number(B), number(X), D is B*X, R = A*D, !. % ((x*2)*3)
sorted(C*X, R) :- sorted(C, A*B), atom(B), number(X), R = A*B*X, !. % ((x*y)*3)
sorted(C*X, R) :- sorted(C, A*B), number(B), atom(X), sorted(A*X, D), R = D*X, !. %((x*2)*y)
sorted(C*X, R) :- sorted(C, A+B), number(B), number(X), D = A*X, E is B*X, R = D+E, !. % ((x+3)*2)
sorted(C*X, R) :- sorted(C, A+B), atom(B), number(X), D = A*X, E = B*X, R = D+E, !. % ((x+y)*2)
sorted(C*X, R) :- sorted(C, A+B), number(B), atom(X), sorted(A*X, D), E = X*B, R = D+E, !. % ((x+3)*y)
sorted(C*X, R) :- sorted(C, A+B), atom(B), atom(X), sorted(A*X, D), sorted(B*X, E), R = D*E, !. % ((x+y)*z)
sorted(C*X, R) :- sorted(C, R1), sorted(X, R2), R = R1*R2, !.

%((x+y)*(w+z))
sorted((C)*(X), R) :- sorted(C, A+B), sorted(X,C+D), R = A*D+B*D, !.
sorted(C*X, R) :- sorted(C, A*B), sorted(X,C+D), R = A*B*C+A*B*D, !.
sorted(C*X, R) :- sorted(C, A*B), sorted(X,C*D), R = A*B*C*D, !.

% Last resort, swap it around
sorted(X*C, R) :- number(X), sorted(C*X, R), !.
sorted(X*C, R) :- atom(X), sorted(C*X, R), !.

% Handle Additions with more than one term in it.
sorted(C+X, R) :- sorted(C, R1), number(R1), number(X), R is R1+X, !.
sorted(C+X, R) :- sorted(C, R1), number(R1), atom(X), R = X+R1, !.
sorted(C+X, R) :- sorted(C, A+B), number(B), number(X), D is B+X, R = A+D, !.
sorted(C+X, A+B+X) :- sorted(C, A+B), atom(A), atom(B), number(X), !.
sorted(C+X, D+B) :- sorted(C, A+B), number(B), atom(X), sorted(A+X, D), !.
sorted(C+X, R) :- sorted(C, R1), sorted(X, R2), number(R1), number(R2), R is R1+R2, !.
sorted(C+X, D+B) :- sorted(C, A+B), atom(B), atom(X), char_code(B,Bc), char_code(X,Xc), Xc =< Bc, sorted(A+X, D), !.
sorted(C+X, A+B+X) :- sorted(C, A+B), atom(A), atom(B), atom(X), !.
sorted(C+X, R1+R2) :- sorted(C, R1), sorted(X,R2), !.
sorted(X, X) :- atom(X), !.
sorted(X, X) :- number(X), !.
sorted(X,X) :- true, !.

% Normalization
simplify(0*_, 0) :- !. 
simplify(_*0, 0) :- !. 

simplify(X, 1*X) :- atom(X), !. % x -> 1*x
simplify(X, X) :- number(X), !. % 3 -> 3
simplify(X*A, A*X) :- atom(X), number(A), !. % x*3 -> 3*x
simplify(A*X, A*X) :- atom(X), number(A), !. % 3*x -> 3*x
simplify(X + Y, 1*X + Y) :- atom(X), number(Y), !. % x + 3 -> 1*x + 3
simplify(X + Y, 1*X + 1*Y) :- atom(X), atom(Y), X \= Y, !. % x + y -> 1*x + 1*y
simplify(X + X, 2*X) :- atom(X), !. % x + x -> 2*x
simplify(X + X*A, B*X) :- atom(X), number(A), B is A + 1, !. % x + x*4 -> 5*x
simplify(X + A*X, B*X) :- atom(X), number(A), B is A + 1, !. % x + 4*x -> 5*x
simplify(A*X + X, B*X) :- atom(X), number(A), B is A + 1, !. % Consider the flip case ... 
simplify(X*A + Y, A*X + Y) :- atom(X), atom(Y), number(A), !.
simplify(A*X + Y, A*X + Y) :- atom(X), atom(Y), number(A), !. % 5*x + y -> 5*x + y
simplify(X + Y*A, X + A*Y) :- atom(X), atom(Y), number(A), !. % x + y*5 -> 5*y + y
simplify(X + A*Y, X + A*Y) :- atom(X), atom(Y), number(A), !. % x + 5*y -> x + 5*y
simplify(A*X + B*X, C*X) :- atom(X), number(A), number(B), C is A + B, !. % 5*x + 4*x -> 9*x
simplify(X + Y, X + Y) :- atom(X), atom(Y), !. % Base case: x + y -> x + y

simplify(A*X + B*Y, C + D) :- simplify(A*X, C), simplify(B*Y, D), !.
simplify(A*X - B*Y, C - D) :- simplify(A*X, C), simplify(B*Y, D), !.

%compounds
simplify(C + X, R + X) :- number(X), simplify(C, R), !.
simplify(A + B, R*X) :- simplify(A, M*X), simplify(B, N*X), atom(X), R is M + N, !.
simplify(A + B, M*X + N*Y) :- simplify(A, M*X), simplify(B, N*Y), atom(X), atom(Y), X \= Y, !.
simplify(A + B, M + R*X) :- simplify(A, M + N*X), simplify(B, S*X), atom(X), R is N + S, !.
simplify(A + B, M + N + SX) :- simplify(A, M + N), simplify(B, SX), !.
simplify(X, X).

% Remove unnecessary terms
clean(X, X) :- number(X), !.
clean(0*_, 0) :- !.
clean(_*0, 0) :- !.
clean(0+X, R) :- clean(X, R), !.
clean(X+0, R) :- clean(X, R), !.
clean(1*X, X) :- atom(X), !.
clean(X*1, X).
clean(1*X, X).
clean(A*X, A*X) :- atom(X), number(A), !.
clean(X + Y, A + B) :- clean(X, A), clean(Y, B), !. 
clean(X,X).






