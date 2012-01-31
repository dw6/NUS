pad_zero(Num, 0, Num) :- !.
pad_zero(NumIn, N, NumOut) :- 
	append(NumIn, [0], NumAux),
	N1 is N-1, 
	pad_zero(NumAux, N1, NumOut).

mult_helper(_Num, 0, _Places, Result, Result) :- !.
mult_helper(Num, 1, Places, ResultIn, ResultOut) :- !,
	pad_zero(Num, Places, NumOut),
	add(ResultIn, NumOut, ResultOut).

mult(_X, [], Res, Res) :- !.

mult(Num1, [H|T], ResultIn, ResultOut) :-
	length([H|T], Len), Places is Len-1,
	mult_helper(Num1, H, Places, ResultIn, ResultAux),
	mult(Num1, T, ResultAux, ResultOut).

mult(Num1, Num2, X) :- mult(Num1, Num2, [0], X).


/* add_helper(A,B, CarryIn, CarryOut, Result). */
add_helper(0,0,0,0,0) :- !.
add_helper(0,0,1,0,1) :- !.
add_helper(0,1,0,0,1) :- !.
add_helper(0,1,1,1,0) :- !.
add_helper(1,0,1,1,0) :- !.
add_helper(1,1,0,1,0) :- !.
add_helper(1,1,1,1,1) :- !.
add_helper(_,_,1,0,1) :- !.
add_helper(_,_,0,0,0) :- !.

/* Normalize the lengths of two inputs, by padding with zeros. */
normalize(InX,InY,OutX,OutY) :-
	length(InX,LenX), length(InY,LenY),
	MaxLen is max(LenX, LenY),
	N1 is MaxLen-LenX,
	N2 is MaxLen-LenY,
	normalize_helper(InX, N1, OutX),
	normalize_helper(InY, N2, OutY).


normalize_helper(List, 0, List) :- !.
normalize_helper(List, N, OutList) :-
	append(List, [0], OutListAux), N1 is N-1,
	normalize_helper(OutListAux, N1, OutList).


add_bin([H1|T1], [H2|T2], CarryIn, RIn, ROut) :- !,
	add_helper(H1, H2, CarryIn, CarryAux, RAux1),
	append([RAux1], RIn, RAux2),
	add_bin(T1,T2,CarryAux,RAux2,ROut).


add_bin([], [], CarryIn, RIn, ROut) :- 
	append(RIn, [CarryIn], ROut). 


add(List1, List2, X) :-
  normalize(List1, List2, OutList1, OutList2),
  add_bin(OutList1, OutList2, 0, [], X).







