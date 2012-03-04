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
  (foreacharg(Arg,Board), param(T)
  do
  (
    (foreacharg(A,Arg),param(T)
    do
    (
      atom(A) -> true; select_val(1,T,A)
    )) 
  )).
  
  

constraints(Board,CarvedPos) :-
  dim(Board,[N,N]),
  no_of_tiles(N,CarvedPos,T), % Find num of tiles. 1..T is therefore the domain.
  ( multifor([X,Y],1,N), param(Board,N,CarvedPos,T)
  do
    (
       
      (not member(X-Y,CarvedPos)) ->   
       (
          V::1..T,subscript(Board,[X,Y],V),[X1,Y1]::1..N,
          (X1 $= X+1, Y1 $= Y, X1 $\= X,   X1 $\= X-1, Y1 $\= Y-1,Y1 $\= Y+1) or
          (X1 $= X-1, Y1 $= Y, X1 $\= X,   X1 $\= X+1, Y1 $\= Y-1,Y1 $\= Y+1) or
          (X1 $= X, Y1 $= Y+1, X1 $\= X-1, X1 $\= X+1, Y1 $\= Y-1,Y1 $\= Y  ) or
          (X1 $= X, Y1 $= Y-1, X1 $\= X-1, X1 $\= X+1, Y1 $\= Y,  Y1 $\= Y+1)
        )
      ; true
    )
  ). 
  


no_of_tiles(N,CarvedPos,Count) :-
  length(CarvedPos,Cs),
  R is (N*N-Cs)/2,
  integer(R,Count).

select_val(K,Number,K) :- K =< Number.
select_val(K,Number,Selection) :-
  K = Number, K1 is K+1,
  select_val(K1,Number,Selection).

delete_all([],_,[]).
delete_all([H|T],A,Result) :- H=A, delete_all(T,A,Result).
delete_all([H|T],A,[H|Result]) :- delete_all(T,A,Result).

% Fill Board with 'x' at Positions
carve_squares(CarvedPos,Board) :-
  foreach(X-Y,CarvedPos),param(Board)
  do ( subscript(Board,[X,Y],x) ).

memberlist([],_).
memberlist([H|T],L) :- member(H,L), memberlist(T,L).

sorted([]).
sorted([_]).
sorted([H1,H2|T]) :- H1 $< H2, sorted([H2|T]).

distinct(K,L) :- length(M,K), sorted(M),memberlist(M,L), memberlist(L,M).

% solve(5,[1-5,4-5,5-1],Result).
