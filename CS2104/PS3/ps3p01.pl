% Problem Set 3, Exercise 1

/*******************************************************************************/
% Sample Run:
% montage(beside(rotate(beside(a,rotate(b))),rotate(beside(rotate(a),b))), out).
/*******************************************************************************/

% Prog  : The series of graphics commands
% Output: The supplied output file name. ("out" -> "out.jpg") 
montage(Prog,Output) :- 
	% Convert term to string
	name(Output, Output_Str), process(Prog, Output_Str, "", _).


% Image   : The series of graphics commands
% FnList  : Current FnList
% Postfix : The Postfix to append the file name to
% OutFn   : FnList + Postfix = OutFn
% process(rotate(rotate(a)), "o", "1", OutFn).
process(Image, FnList, Postfix, OutFn) :-
	Image =.. [F, Image1],
	F = rotate, 
	append([FnList, Postfix], OutFnList), 
	name(OutFn, OutFnList),
	process(Image1, OutFnList, "1", OutFn1),
	write('convert -rotate 90 '),
	write(OutFn1), write('.jpg'), write(' '), write(OutFn), writeln('.jpg'), !.


process(Image, FnList, Postfix, OutFn) :-
	Image =.. [F, Image1, Image2],
	F = beside,
	append([FnList, Postfix], OutFnList), 	 % Construct file name
	name(OutFn, OutFnList),
	process(Image1, OutFnList, "1", OutFn1), % Evaluate Image 1.
	process(Image2, OutFnList, "2", OutFn2), % Evaluate Image 2.
	write('convert +append '),
	write(OutFn1), write('.jpg '), write(OutFn2), write('.jpg '), 
	write(OutFn), writeln('.jpg'), !.


% Base case: 
% Do not use the filename we have calculated
% Scale down the image created
process(Image, FnList, Postfix, OutFn) :-
	atom(Image),
	append([FnList, Postfix], OutFnList), !,
	name(OutFn, OutFnList),
	write('convert -scale 50%%x50%% '),
	write(Image), write('.jpg'), write(' '), write(OutFn), writeln('.jpg'), !.

