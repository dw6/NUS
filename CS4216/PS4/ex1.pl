% L = [S,E,N,D,M,O,R,Y], 
% L::0..9, alldifferent(L), 
% S$\=0, M$\=0, 
% 1000*S+100*E+10*N+D+1000*M+100*O+10*R+E $= 10000*M+1000*O+100*N+10*E+Y,
% labeling(L).



% crypto([G,E,R,A,L,D]+[D,O,N,A,L,D]=[R,O,B,E,R,T]).

crypto(L1+L2=L3) :-
	flatten([L1,L2,L3],L4).


