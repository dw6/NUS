:-lib(ic).

gentree(Node,OutList) :-
	Node =..[_,Child],!,
	gentree(Child,ChildList),
	append([[_,_]],[ChildList],OutList).

gentree(Node,OutList) :-
	Node =..[_,LChild,RChild],!,
	gentree(LChild,LChildList),
	gentree(RChild,RChildList),
	append([[_,_]],[LChildList,RChildList],OutList).

gentree(Node,[[_,_]]) :- atom(Node).

traverse(Depth,L,Leaf,NewLeaf,[[X]]) :- 
	atom(Leaf),Y is Depth*L, NewLeaf =.. [Leaf,X,Y].

traverse(Depth,L,Tree,NewTree,XList) :-
	Tree =.. [Node|Children],
	Depth1 is Depth+1,
	Y is Depth*L,
	(
		fromto((Children,[],Args),
					 ([C|T],VarIn,[Arg|ArgOut]),
					 (T,VarOut,ArgOut),
					 ([],VarList,[])
		),param(Depth1,L) do
		traverse(Depth1,L,C,Arg,Vs),
		combine_list(VarIn,Vs,VarOut)
	),
	append(Args,[X,Y],NewArgs),
	align_center(X,VarList),
	NewTree =.. [Node|NewArgs],
	XList = [[X]|VarList].

% % Y Distance
% par_child_constr(Tree,Y) :-

% 	Tree = [[PX,PY],[[LX,LY]|_],[[RX,RY]]|_],
% 	PX $= (RX/2 - LX/2),
% 	LY $= RY,
% 	LY - PY $= Y,
% 	writeln('2 Children'),
% 	par_child_constr([[LX,LY]|_],Y),
% 	par_child_constr([[RX,RY]|_],Y).

% par_child_constr(Tree,Y) :-
% 	Tree = [[PX,PY],[[CX,CY]|_]],
% 	PX $= CX,
% 	CY - PY $= Y,
% 	writeln('1 Child'),
% 	par_child_constr([[CX,CY]|_],Y).

% par_child_constr(_,_) :-
% 	writeln('Base case').

% depth(List,Depth) :- depth(List,0,DepthAux).
% depth(List,Depth,Depth) :- 
% 	flatten(List,Flat), 
% 	flatten(Depth, List, Flat), !.
% depth(List,DepthIn,DepthOut) :- 
% 	DepthAux is DepthIn+1,
% 	depth(List,DepthAux,DepthOut).
