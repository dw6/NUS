% Benjamin Tan Wei Hao
% U077129N
% Fractal Fun

% Behavior is similar to how the '+' operator works
:- op(499,xfy,;).

% Main Predicate to call
fractal(X = (Expr), Level, Length) :-
	writeln('from turtle import *'),
	writeln('import time'),
	writeln('s = Screen()'),
	writeln('s.setworldcoordinates(0,-500, 1500, 1000)'),
	writeln('delay(0)'),
	fractal(X, Expr, Level, Length, Expr).

fractal(X, X;Ee, Level, Length, StoredExpr) :-
	Level > 1,
	Level1 is Level-1, 
	% Recurse one level down
	fractal(X, StoredExpr, Level1, Length, StoredExpr),
	fractal(X, Ee, Level, Length, StoredExpr).


fractal(X, E;Ee, Level, Length, StoredExpr) :-
	Level > 1, 
	E =.. [Cmd, Angle], member(Cmd,[left,right]), 
	write(Cmd), write('('), write(Angle), writeln(')'),
	fractal(X, Ee, Level, Length, StoredExpr).


fractal(_X, E, Level, _Length, _StoredExpr) :-
	Level > 1, 
	E =.. [Cmd, Angle], member(Cmd,[left,right]), 
	write(Cmd), write('('), write(Angle), writeln(')').


fractal(X, X, Level, Length, StoredExpr) :-
	Level > 1, 
	Level1 is Level-1,
	fractal(X, StoredExpr, Level1, Length, StoredExpr).


fractal(X, _Expr, 1, Length, StoredExpr) :-
	fractal_helper(X, StoredExpr, Length).

fractal(_X, _Expr, _, _Length, _StoredExpr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper predicates to build one "unit"
fractal_helper(X, X;Ss, Length) :-
	write('forward('), write(Length), writeln(')'),
	fractal_helper(X, Ss, Length).

fractal_helper(X, S;Ss, Length) :-
	S =.. [Cmd, Angle], member(Cmd,[left,right]), 
	write(Cmd), write('('), write(Angle), writeln(')'),
	fractal_helper(X, Ss, Length).

fractal_helper(_X, S, _Length) :-
	S =.. [Cmd, Angle], member(Cmd,[left,right]), 
	write(Cmd), write('('), write(Angle), writeln(')').

fractal_helper(X, X, Length) :-
	write('forward('), write(Length), writeln(')').


% :- tell('run.py'),
%    fractal(g=(left(30);g;right(120);g;left(120);g;right(30)),5,20), 
%    writeln('time.sleep(1000)'), told.


% :- tell('run.py'),
%    fractal(g = (left(120);g;right(60);g;right(60);g;right(60);g;g;left(60)),10,10), 
%    writeln('time.sleep(1000)'), told.

:- tell('run.py'),
   fractal(g = (g; left(45);g;right(90);g;left(45);g),4,5), 
   writeln('time.sleep(1000)'), told.