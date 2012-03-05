% Benjamin Tan Wei Hao
% U077129N

:- lib(ic).

solve(Number,CarvedPos,Board) :-
  dim(BoardAux,[Number,Number]),         
  constraints(BoardAux,CarvedPos,Vars),   
  search(Vars,0,input_order,indomain,complete,[]),
  replacex(CarvedPos,BoardAux,Board).

constraints(Board,CarvedPos,Vars) :-
  dim(Board,[N,N]),
  num_doms_and_tiles(N,CarvedPos,DomCount,TilesCount),
  length(Vars,TilesCount),					
  ( 
    multifor([X,Y],1,N),fromto(Vars,InV,OutV,[]),param(Board,CarvedPos,DomCount,N) do
    (
      member((X-Y),CarvedPos) ->
        (OutV = InV, subscript(Board,[X,Y],0));
        (
          subscript(Board,[X,Y],V), 
          InV = [V|OutV], V::1..DomCount, 
          ((X<N) -> subscript(Board,[X+1,Y],D); D = -1), 
          ((X>1) -> subscript(Board,[X-1,Y],U); U = -2),
          ((Y<N) -> subscript(Board,[X,Y+1],R); R = -3), 
          ((Y>1) -> subscript(Board,[X,Y-1],L); L = -4),
          alldifferent([D,U,R,L]),  alldifferent2(Board,CarvedPos,(X-Y))
        )
    )
  ), distinct(DomCount,Vars).

replacex(CarvedPos,BoardAux,Board) :-
	dim(BoardAux,[N,N]),
	dim(Board,[N,N]),
	(multifor([I,J],1,N), param(BoardAux,CarvedPos,Board) do
		member((I-J),CarvedPos) -> 
			subscript(Board,[I,J],x);
			subscript(Board,[I,J],V), subscript(BoardAux,[I,J],V)
	).

alldifferent2(Board,CarvedPos,(I-J)) :-
	dim(Board,[N,N]),
	(multifor([X,Y],1,N),param(I,J,CarvedPos,Board) do 
		(
			neighbor((X-Y),(I-J)) -> true ; 
				(
					member((X-Y),CarvedPos) -> true ;
					(
						subscript(Board,[X,Y],V1), 
						subscript(Board,[I,J],V2), 
						V1 $\= V2
					)
				)	
		)
	).

neighbor((X-Y),(I-J)) :-
	(((X=:=I),(Y-J > -2,Y-J < 2));((Y=:=J),(X-I> -2,X-I < 2))).

num_doms_and_tiles(N,CarvedPos,DomCount,TilesCount) :-
  length(CarvedPos,Cs),
  TilesCount is (N*N-Cs),
  R is (N*N-Cs)/2,
  integer(R,DomCount).


select_val(1,N,Col) :-
  ( fromto(fail,C,(C;(Col=I)),Q),for(I,1,N),param(Col) do true ),Q.

% Suspended member.
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
