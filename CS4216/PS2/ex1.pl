% Benjamin Tan Wei Hao
% Problem Set 2 - Exercise 1
:- dynamic p/1.
:- dynamic s/1.
:- dynamic r/1.
:- dynamic q/1.
:- dynamic t/1.
 
solve(true,_CS) :- !.
solve((A,B),CS) :- !, solve(A,CS), solve(B,CS).
solve(H,CS) :- clause(H,Body), 
							  (peek(H,CS) ->
								(writeln('Infinite loop detected, execution aborted.'), abort); solve(Body,[H|CS])).

% infinite loop iff the first guy on the stack is the function call
peek(H,[H1|_]) :- H1 == H.
peek(H,[H1]) :- H1 == H.

r(_) :- true.
p(_) :- true.
s(X) :- r(X), p(X),r(X), p(X),r(X), p(X).
q(X) :- r(X), p(X),r(X), q(X), p(X),r(X), p(X). % infinite call to q
t(X) :- s(X), r(X), s(X),s(X), r(X), s(X),s(X), q(X), s(X). % calls q, the troublemaler

