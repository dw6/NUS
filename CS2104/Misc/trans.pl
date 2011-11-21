%%%
% Convert pre/post increment statements and translate them to non post/pre increment ones.
% Only the multiplication is needed.
%%%

% Main Idea

% Go through the entire expression tree, collecting pre/post increment expressions
% in a list. At the same time, we build up a substituted expression, replace all the
% pre/post increments with only the variables. 
% Lastly, we simply combine the [pre] + [substituted expression] + [post]

:- op(600, fy, ++#).
:- op(600, yf, #++).

% Main Predicate 
translate((X=Expr), Code) :-
	translate_helper(Expr, ModExpr, [], PreList, [], PostList),
	% Combine the pre-increment list, substituted expressions, post-increment list
	Code = (PreList, X=ModExpr, PostList).

% Handle multiplication expressions
translate_helper(Expr, ModExpr, PreIn, PreOut, PostIn, PostOut) :-
	Expr =.. [*, X, Y],
	translate_helper(X, ModExprX, PreIn, PreAux, PostIn, PostAux), 
	translate_helper(Y, ModExprY, PreAux, PreOut, PostAux, PostOut),
	ModExpr =.. [*, ModExprX, ModExprY].

% Add to Post-increment list
translate_helper(A#++, A, Pre, Pre, PostIn, PostOut) :-
	append(PostIn, [(A=A+1)], PostOut).

% Add to Pre-increment list
translate_helper(++#A, A, PreIn, PreOut, Post, Post) :-
	append(PreIn, [(A=A+1)], PreOut).

% No change for any other term
translate_helper(A, A, Pre, Pre, Post, Post).

% Sample run   
:- InCode = (x=(a#++)*(++#b)), translate( InCode, OutCode), 
   write('In : '), writeln(InCode), write('Out : '), writeln(OutCode),nl.

:- InCode = (x=(a#++)*(b#++)*(++#c)*(d#++)*y*(++#e)*z), translate( InCode , OutCode), 
   write('In : '), writeln(InCode), write('Out : '), writeln(OutCode),nl.
