% <S> ::=  ‘(‘ <A> ‘)’
% <A> ::=  ‘[‘ <A> ‘]’
% 		 | ‘{‘ <A> ‘}’ <S> 
%	     |a|...| z
expr(S) :- S = [_|_], S1 = [_|_], append(["(", S1, ")"], S), !, subexpr(S1), !.

subexpr(S) :- S = [_|_], S1 = [_|_], append(["[", S1, "]"], S), !, subexpr(S1), !.

subexpr([S]) :- 97 =< S, S =< 122, !.		  

subexpr(S) :- S = [_|_], S1 = [_|_], S3 = [_|_], 
			  append([S2, S3], S),  append(["{", S1, "}"], S2), 
			  subexpr(S1), expr(S3), !. 


:- expr("(a)").
:- expr("({a}(a))").
:- expr("({{a}({a}({a}(a)))}([{a}(a)]))").