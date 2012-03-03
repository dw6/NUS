% Benjamin Tan Wei Hao
% U077129N

% N-Queens problem, only difference now is that our pieces are God Like! => King + Rook + Knight => Knooking

:- lib(suspend).

Knooking(Struct,Number) :-
	dim(Struct,[Number]),
	constraints(Struct,Number),
	search(Struct).

constraints(Struct,Number) :-
	( for(I,1,Number),													% We iterate forwards (I is the counter) 
		param(Struct,Number)   										% We need to use this inside the loop
	do
		Struct[I] :: 1..Number, 									% Each piece is in the board
		( for(J,1,I-1), 													% Iterate from 1 to the (I-1)th column
			param(I,Struct)													%	to make sure that all the (I-1) cols satisfy constraints
		do
			Struct[I] $\= Struct[J], 																										% Rook
			(I-J =:= 1 -> 													
				(Struct[I]-Struct[J] $\= 1, Struct[I]-Struct[J] $\= -1,         % King
				 Struct[I]-Struct[J] $\= 2, Struct[I]-Struct[J] $\= -2); true), % Knight
			(I-J =:= 2 -> 													
				(Struct[I]-Struct[J] $\= 1,Struct[J]-Struct[I] $\= 1); true)    % Knight
		)
	).

search(Struct) :-
	dim(Struct,[N]),
	(foreacharg(Col,Struct),
		param(N)
	do
		select_val(1,N,Col)
	).

select_val(1,N,Col) :-
	(fromto(fail,C,(C;(Col=I)),Q), for(I,1,N), param(Col) do true), Q.

% No solutions for 1..7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example Run:
%
% Knooking(Struct,8).
%
% Struct = [](1, 4, 7, 2, 5, 8, 3, 6)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%








