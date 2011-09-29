% Fractal Fun

:-op(900,yf,;).

% Main Predicate to call
fractal(X = (X), _Level, _Length) :-
	writeln('Base Case'), !.

fractal(X = (Expr), Level, Length) :-
	fractal(X, Expr, Level, Length, Expr).

fractal(_X, _, 0, Length, _StoredExpr) :-
	write('forward('), write(Length), writeln(')').
 
% X      	 : Graph object (Used to check the equation for syntax errors)
% Expr   	 : Remaining expression
% StoredExpr : Stored Expression that we used.
fractal(X, E;Ee, Level, Length, StoredExpr) :-
	E =.. [right, Angle],
	write('right('), write(Angle), writeln(')'),
	fractal(X, Ee, Level, Length, StoredExpr).

fractal(X, E;Ee, Level, Length, StoredExpr) :-
	E =.. [left, Angle],
	write('left('), write(Angle), writeln(')'),
	fractal(X, Ee, Level, Length, StoredExpr).

fractal(X, E;_, Level, Length, StoredExpr) :-
	writeln(E),
	E = X, 
	Level1 is Level-1,
	fractal(X, StoredExpr, Level1, Length, StoredExpr).

:- fractal(g = (g;left(45);g),3,30).
% :- fractal(g = (g;left(45);g;right(90);g;left(45);g),3,30).