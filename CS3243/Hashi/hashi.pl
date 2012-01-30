/**************************************************************************************** 
   1. They must begin and end at distinct islands, travelling a straight line in between.  [Done]
   2. They must not cross any other bridges or islands.  								   [Not Done]
   3. They may only run perpendicularly.  												   [Done]
   4. At most two bridges connect a pair of islands.  									   [Done]
   5. The number of bridges connected to each island must match the number on that island. [Done]
   6. The bridges must connect the islands into a single connected group.   			   [Done]
****************************************************************************************/

:- dynamic bridge/2.											

% Create Islands dynamically
createIslands([]).
createIslands([Head|Tail]):-
	assert(island(Head)),
	createIslands(Tail).

/**************************************************
 	Computing adjacencies : Left, Right, Up, Down
 **************************************************/

computeLeftAdjacency(_,[[_,Y],_]):- Y < 0, !.    						% Base case

computeLeftAdjacency([[Ori_X,Ori_Y],_], [[X,Y],_]):-				
	LIdx is Y-1,																							% Search to the left
	((island([[X,LIdx],_]), 																	% Check that island exists, x-coordinates are the same			
	  assert(adjacent([[Ori_X,Ori_Y], [X,LIdx]])), !);				% add adjacent(X,Y) to KB	
	  computeLeftAdjacency([[Ori_X, Ori_Y],_] , [[X,LIdx],_])).

computeRightAdjacency(_,[[_,Y],_]):-     % Base case
	gridSize(_,Rlimit),
	Y > Rlimit.	
	
computeRightAdjacency([[Ori_X,Ori_Y],_], [[X,Y],_]):-	
	RIdx is Y+1,																							% Search to the right
	((island([[X,RIdx],_]),																		% Check that island exists, x-coordinates are the same		
     assert(adjacent([[Ori_X,Ori_Y],[X,RIdx]])), !);	 			% add adjacent(X,Y) to KB
	computeRightAdjacency([[Ori_X,Ori_Y],_], [[X,RIdx],_])).				

/***/ 	

computeUpAdjacency(_,[[X,_],_]):- X < 0, !.   							% Base case

computeUpAdjacency([[Ori_X,Ori_Y],_], [[X,Y],_]):-				
	UpIdx is X-1,																							% Search upwards
	((island([[UpIdx,Y],_]), 																	% Check that island exists, y-coordinates are the same			
	  assert(adjacent([[Ori_X,Ori_Y], [UpIdx,Y]])), !);				% add adjacent(X,Y) to KB
	  computeUpAdjacency([[Ori_X, Ori_Y],_] , [[UpIdx,Y],_])).

/***/ 	

computeDownAdjacency(_,[[X,_],_]):-   											% Base case
	gridSize(Llimit,_),
	X > Llimit.	

computeDownAdjacency([[Ori_X,Ori_Y],_], [[X,Y],_]):-				
	DnIdx is X+1,																							% Search downwards
	((island([[DnIdx,Y],_]), 																	% Check that island exists, y-coordinates are the same	
      assert(adjacent([[Ori_X,Ori_Y], [DnIdx,Y]])), !);			% add adjacent(X,Y) to KB					
	  computeDownAdjacency([[Ori_X, Ori_Y],_] , [[DnIdx,Y],_])).

/***/ 	

% Calculates the adjacency in all 4 directions
computeAdjacency([[X,Y],_]):-
	computeLeftAdjacency([[X,Y],_], [[X,Y],_]),
	computeRightAdjacency([[X,Y],_], [[X,Y],_]),
	computeUpAdjacency([[X,Y],_], [[X,Y],_]),
	computeDownAdjacency([[X,Y],_], [[X,Y],_]),!.

% Calculates the adjacencies of all vertices
% E.g. : shima(X),computeAdjacencies(X).
computeAdjacencies([], AdjacencyList) :- 	
	findall([[[W, X],1], [[Y, Z],1]], adjacent([[W, X], [Y, Z]]), AdjacencyList), % Collect all the adjacent(X,Y) predicates ...
	assert(adjacencyList(AdjacencyList)). 								 		  									% ... store them into AdjacencyList

computeAdjacencies([Head|Tail], _):-
	computeAdjacency(Head),
	computeAdjacencies(Tail, _).


/***************************
 	Crossing Constraints 
 ***************************/
crossingConstraint([[X1,X2],_],[[Y1,Y2],_]):-
	adjacent([[X1,X2], [Y1,Y2]]),																		% Islands should be beside each other (and not be the same island)
	countBridgesBetweenIslands([[X1,X2],_], [[Y1,Y2],_], Count),    % check that we do not exceed the bridges ...
	Count < 2,																											% that we connect to each other
	island([[X1,X2],N1]), island([[Y1,Y2],N2]), 	 	
	N1 > 0, N2 > 0,																									% have at least once bridge to connect
	assert(bridge([[X1,X2],N1],[[Y1,Y2],N2])),
	M1 is N1-1, M2 is N2-1,																					% islands have now one less bridge to connect
	assert(island([[X1,X2],M1])),																		% update Island(X,Y) to reflect one less bridge
	retract(island([[X1,X2],N1])),
	assert(island([[Y1,Y2],M2])),
	retract(island([[Y1,Y2],N2])).		


% Count the number of bridges originating from Island
% countBridges(Island, Count).
% countBridges([[0, 0],1], Count).

countBridges(Island, Count):-
	findall(Island, bridge(Island, _), Result1), length(Result1, Count).
crossingConstraints([]).
crossingConstraints([[[[A1,A2],N1],[[B1,B2],N2]]|Tail]):-
	crossingConstraint([[A1,A2],N1], [[B1,B2],N2]);
	crossingConstraints(Tail).



% Count the number of bridges between 2 islands
% countBridges(Island, Count).
countBridgesBetweenIslands([[A1,A2],_],[[B1,B2],_], Count):-
	findall([[A1,A2],[B1,B2]], bridge([[A1,A2],_],[[B1,B2],_]), Result), length(Result,Count).


% Final solver.
hashi:-
	shima(Islands),																								% Unify the list of islands with "Islands"
	createIslands(Islands),    																		% Create the islands by unifiying "Islands"
	computeAdjacencies(Islands, _),																% Compute adjacency list
	adjacencyList(X), 																						% use the computed adjacency list
	crossingConstraints(X).																				% to calculate the crossing constraints

% print bridges
solve:-
	findall(_,hashi,_),
	bridge([[X1,X2],_],[[Y1,Y2],_]),
	write(X1),write(','),write(X2),write('|'),write(Y1),write(','),write(Y2),nl.

