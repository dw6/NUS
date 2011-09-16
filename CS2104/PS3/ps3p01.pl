% Problem Set 3, Exercise 1

% Image   : The Image we are interested in.
% Fn      : Current File Name
% Postfix : The Postfix to append the file name to
% OutFn   : Output Filename

process(Image, FnList, Postfix, OutFn) :-
	Image =.. [F, Image1],
	F = rotate, 
	append([FnList, Postfix], OutFnList), 
	name(OutFn, OutFnList),
	process(Image1, OutFnList, "1", OutFn1),
	write('convert -rotate 90 '),
	write(OutFn1), write('.jpg'), write(' '), write(OutFn), writeln('.jpg'), !.



% Image   : The Image we are interested in.
% Fn      : Current File Name
% Postfix : The Postfix to append the file name to
% OutFn   : Output Filename
% For Rotate Base Case
% convert -rotate 90 o1121.jpg o112.jpg

% Sample Call: process(a, "o1", "1", OutFn). 
% Base case we do not use the Filename we have calculated
process(Image, FnList, Postfix, OutFn) :-
	atom(Image),
	append([FnList, Postfix], OutFnList), !,
	name(OutFn, OutFnList),
	write('convert -scale 50%%x50%% '),
	write(Image), write('.jpg'), write(' '), write(OutFn), writeln('.jpg'), !.










% rotate(Image) :- 
	
% 	write('convert -rotate 90 '),
% 	write(Image), write('.jpg '),
% 	atom_chars(Image, ImageFName),
% 	reverse(ImageFName, L), L = [_|T], 	
% 	reverse(T, NewFName), 
% 	atomic_list_concat(NewFName, OutputImage),
% 	write(OutputImage), writeln('.jpg').


% beside(Image1, Image2) :- 

% 	% Scale first
% 	write('convert -scale 50%%50%% '),
% 	write(Image1), write('.jpg '),
% 	write(Image2), writeln('.jpg '),


% 	% Then append
% 	write('convert +append '),
% 	write(Image1), write('.jpg '),
% 	write(Image2), write('.jpg '),
% 	atom_chars(Image1, ImageFName),
% 	reverse(ImageFName, L), L = [_|T], 	
% 	reverse(T, NewFName), 
% 	atomic_list_concat(NewFName, OutputImage),
% 	write(OutputImage), writeln('.jpg'),
% 	OutputImage.

	
