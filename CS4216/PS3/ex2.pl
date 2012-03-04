% Benjamin Tan Wei Hao
% U077129N
% :- lib(ic).
:- lib(suspend).


% Start with a board without any crosses yet
solve(Number,CarvedPos,Board) :-
  dim(Board,[Number,Number]),         % Create Board
  carve_squares(CarvedPos,Board),     % Carved squares out
  constraints(Board,CarvedPos),       % Generate constraints
  search(Board,CarvedPos).

% Convert multidimentional array to a List of Lists.
convert(Board,BoardList) :-
  dim(Board,[N,N]),
  length(BoardList,N),
  ((foreacharg(Row,Board),fromto(BoardList,[L|T],T,[]))
    do
      flatten_array(Row,L)
  ).

search(Board,CarvedPos) :-
  dim(Board,[N,N]),
  no_of_tiles(N,CarvedPos,T),
  flatten_array(Board,FlatBoard),
  (foreacharg(Arg,FlatBoard), param(FlatBoard,T)
  do
    select_val(1,T,Arg)
  ),
  writeln(Board).

constraints(Board,CarvedPos) :-
  dim(Board,[N,N]),
  no_of_tiles(N,CarvedPos,T), % Find num of tiles. 1..T is therefore the domain.
  ( multifor([X,Y],1,N), param(Board,N,T)
  do
    flatten_array(Board,FlatBoard), FlatBoard::1..T,
    (
     ((X1 $= X+1, Y1 $= Y);
      (X1 $= X-1, Y1 $= Y);
      (X1 $= X, Y1 $= Y+1);
      (X1 $= X, Y1 $= Y-1))
     ,[X1,Y1]::1..N,
      not member(X-Y,CarvedPos), 
      not member(X1-Y1,CarvedPos)) ->   % None of the positions are on an 'x'
      Board[X,Y] $= Board[X1,Y1]
    ; true
  ).


no_of_tiles(N,CarvedPos,Count) :-
  length(CarvedPos,Cs),
  R is (N*N-Cs)/2,
  integer(R,Count).

select_val(K,Number,K) :- K =< Number.
select_val(K,Number,Selection) :-
  K = Number, K1 is K+1,
  select_val(K1,Number,Selection).


% Fill Board with 'x' at Positions
carve_squares(CarvedPos,Board) :-
  foreach(X-Y,CarvedPos),param(Board)
  do ( subscript(Board,[X,Y],x) ).

% solve(5,[1-5,4-5,5-1],Result).
