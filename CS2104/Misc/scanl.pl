% Write a Prolog procedure that takes in a list of integers, 
% and succeeds if the numbers in the list are consecutive, 
% and fails otherwise.

consec([]).
consec([_]).

consec([H1|T]) :-
   T = [H2|_],
   H2 is H1+1, 
   consec(T).  

:- consec([2,3,4,5,7]).

% Simulates scanl of Haskell in Prolog

scanl([], _, Out) :- writeln(Out).

scanl([H|T], Acc, In) :-
   Acc1 is H + Acc,
   append(In, [Acc1], Aux),
   scanl(T, Acc1, Aux).

scanl([H|T]) :- scanl([H|T], 0, []).  

:- scanl([1,2,3,4,5,6,7,8,9,10]).

