% 2.01 (**) Determine whether a given integer number is prime. 
is_prime(2).
is_prime(3).
is_prime(P) :- integer(P), P > 3, P mod 2 =\= 0, \+ has_factor(P,3).  

% has_factor(N,L) :- N has an odd factor F >= L.
%    (integer, integer) (+,+)

has_factor(N,L) :- N mod L =:= 0.
has_factor(N,L) :- L * L < N, L2 is L + 2, has_factor(N,L2).

% X is the current element we are visiting.
primeList(Primes, _, _, 0):- writeln(Primes).

primeList(In, Out, X, N) :-
	(is_prime(X) -> (append(In, [X], Out), N1 is N-1) ; (Out = In, N1 is N)),
	X1 is X + 1,
	primeList(Out, _, X1, N1 ).

primes(N) :- primeList([], _, 2, N).

:- primes(100).