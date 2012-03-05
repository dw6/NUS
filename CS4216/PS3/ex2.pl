% Benjamin Tan Wei Hao
% U077129N

:- lib(ic).

% Start with a board without any crosses yet
solve(Number,CarvedPos,Board) :-
  dim(Board,[Number,Number]),         % Create Board
  % carve_squares(CarvedPos,Board),     % Carved squares out
  constraints(Board,CarvedPos,Vars),       % Generate constraints
  search(Vars,0,input_order,indomain,complete,[]).

% Convert multidimentional array to a List of Lists.
convert(Board,BoardList) :-
  dim(Board,[N,N]),
  length(BoardList,N),
  (foreacharg(Row,Board), fromto(BoardList,[L|T],T,[])
  do ( flatten_array(Row,L) )
  ).


constraints(Board,CarvedPos,Vars) :-
  dim(Board,[N,N]),
  length(CarvedPos,C),
  no_of_tiles(N,CarvedPos,T),
  CellCount is (N*N)-C,
  length(Vars,CellCount),
  ( 
    multifor([X,Y],1,N),fromto(Vars,InV,OutV,[]),param(Board,CarvedPos,T,N) do
    (
      member((X-Y),CarvedPos) ->
        (OutV = InV, subscript(Board,[X,Y],0));
        (
          subscript(Board,[X,Y],V),
          InV = [V|OutV], V::1..T, 
          ((X<N) -> subscript(Board,[X+1,Y],D); D = -1), 
          ((X>1) -> subscript(Board,[X-1,Y],U); U = -2),
          ((Y<N) -> subscript(Board,[X,Y+1],R); R = -3), 
          ((Y>1) -> subscript(Board,[X,Y-1],L); L = -4),
          alldifferent([D,U,R,L])
        )
    )
  ), distinct(T,Vars).

  
no_of_tiles(N,CarvedPos,Count) :-
  length(CarvedPos,Cs),
  R is (N*N-Cs)/2,
  integer(R,Count).


delete_all([],_,[]).
delete_all([H|T],A,Result) :- H=A, delete_all(T,A,Result).
delete_all([H|T],A,[H|Result]) :- delete_all(T,A,Result).


% Fill Board with 'x' at Positions
carve_squares(CarvedPos,Board) :-
  foreach(X-Y,CarvedPos),param(Board)
  do ( subscript(Board,[X,Y],x) ).

% Aux Functions

select_val(1,N,Col) :-
  ( fromto(fail,C,(C;(Col=I)),Q),for(I,1,N),param(Col) do true ),Q.

sus_member(E,L) :- sus_member(E,L,0).
sus_member(_,[],C):- C.
sus_member(E,[H|T],C):- sus_member(E,T,C or (E $= H)).

memberlist([],_).
memberlist([H|T],L) :- sus_member(H,L), memberlist(T,L).

distinct(K,L) :-
  length(M,K),
  (for(I,1,K),foreach(A,M) do A=I),
  memberlist(M,L),
  memberlist(L,M).

% solve(5,[1-5,4-5,5-1],Result).
