% Benjamin Tan Wei Hao
% U077129N
% Fractal Fun

% Generates a python script (run.py) that displays the diagram.
% Note: Use Python3. 

:- op(499,xfy,;).

% Main Predicate to call
fractal(X = (Expr), Level, Unit) :-
	writeln('from turtle import *'),
	writeln('import time'),
	writeln('s = Screen()'),
	writeln('s.setworldcoordinates(0,-500, 1500, 1000)'),
	writeln('delay(0)'),
	writeln('def g(level,unit) :'),
	writeln('	if level == 0 :'),
	writeln('		forward(unit)'),
	writeln('	else :'),
	fractal(X, Expr, Unit),
	write('g('), write(Level), write(','), write(Unit), writeln(')').

fractal(X, E;Ee, Unit) :-
	E =.. [Cmd,Angle], member(Cmd,[left,right]),
	write('		'),write(Cmd),write('('),write(Angle),writeln(')'),
	fractal(X, Ee, Unit).

fractal(X, X;Ee, Unit) :-
	write('		'),write(X), writeln('(level-1,unit)'),
	fractal(X,Ee, Unit).

fractal(X, X, _Unit) :-
	write('		'),write(X), writeln('(level-1,unit)').

fractal(_X, E, _Unit) :-
	E =.. [Cmd,Angle], member(Cmd,[left,right]),
	write('		'),write(Cmd),write('('),write(Angle),writeln(')').


% Test your expressions here.
:- tell('run.py'),
   fractal(g=(left(30);g;right(120);g;left(120);g;right(30)),5,20), 
   writeln('time.sleep(1000)'), told.