permute([],[]) :- !.
permute([H|T], Result) :- permute(T, X), delete(H, Result, X).
