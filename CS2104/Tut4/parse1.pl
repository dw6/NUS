expr(S) :-
	constrain(S,S2,[],[S1,S2],["+","-"]),
	!,subexpr(S1), term(S2).

subexpr("") :- !.
subexpr(S) :-
	constrain(S,S2,[O],[S1,S2,[O]],["+","-"]),
	!,subexpr(S1),term(S2).

term(S) :-
	constrain(S,S2,[],[S1,S2],["*","/"]),
	!,subterm(S1), factor(S2).

subterm("") :- !.
subterm(S):-
	constrain(S,S2,[O],[S1,S2,[O]],["*","/"]),
	!,subterm(S1),factor(S2).

factor(S) :-
	constrain(S,S1,[],[S1,S2],["^"]),
	!,base(S1), restexp(S2).

restexp("") :- !.
restexp(S) :-
	constrain(S,S1,"^",["^",S1,S2],["^"]),
	!, base(S1), restexp(S2).

base(S) :- append(["(",S1,")"],S), !, expr(S1).
base([S]) :- 97 =< S, S =< 122.

constrain(S,S1,O,L,OL) :-
	S1 = [_|_], % S is a list
	append(L,S), 
	balanced(S1,R1),
	findall(X,(member([X],OL),member(X,R1)),[]),
	(   O \= [] -> member(O,OL) ; true ).

balanced("","") :- !. 								 % Trivial case

balanced(S,"") :-									 % Match (S1)
	append(["(",S1,")"],S),balanced(S1,_),!.		
balanced(S,R) :-									 
	append([X],S1,S), \+ member([X],["(",")"]),!,    % Front "("
	balanced(S1,R1), append([X],R1,R). % writeln(R).
balanced(S,R) :-
	append(S1,[X],S), \+ member([X],["(",")"]),!,	 % Back ")"
	balanced(S1,R1), append(R1,[X],R). %, writeln(R).
balanced(S,R) :-
	append(["(",S1,")",S2,"(",S3,")"],S),
	balanced(S1,_),balanced(S2,R),balanced(S3,_), writeln(S1), writeln(S2), writeln(S3).




