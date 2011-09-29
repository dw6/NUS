% We augment the toy language discussed in Lecture 6 with arrays. All arrays have 100 
% elements. Array access is done via the operator @. That is, the expression a@k accesses 
% the element with rank k in array a. Array indices range from 0 to 99. Augment the rules 
% for compiling expressions with array access. Test the compiler with a sorting algorithm.

:- op(1099,yf,;).
:- op(960,fx,if).
:- op(959,xfx,then).
:- op(958,xfx,else).
:- op(300,xfx,@). 											% Introduce the array operator
:- op(960,fx,while).
:- op(959,xfx,do).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Handle array declarations %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compileExpr(A@K,Ein,Eout,Tin,Tout) :-
	atom(A), !,
	(   member((A->Addr),Ein)	
	->  Tout = Tin, Eout = Ein
	;   
	(Tout is Tin+400, Eout = [(A->Tin)|Ein], Addr = Tin),nl),
	% Get the base address of the array
	member((A->ArrAddr),Eout), 						
	% Handle 2 different cases: K might be an integer or a variable
	(
		integer(K) -> ( 
			ArrAddr1 is (ArrAddr + K * 4), 		
			write('    ecx = *(int*)&M['),write(ArrAddr1), write('] ; // save value of '), writeln(K)
		);
		(
			member((K->OffsetAddr),Ein),						
			ArrAddr1 = (ArrAddr + OffsetAddr * 4), 
			write('    ecx = *(int*)&M['),write(OffsetAddr), write('] ; // save value of '), writeln(K)
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compileExpr(K,E,E,T,T) :-
	integer(K),!,
	write('    esp -= 4 ; *(int*)&M[esp] = '),
	write(K),write(' ; // push '), writeln(K).

compileExpr(V,Ein,Eout,Tin,Tout) :-
	atom(V),!,
	(   member((V->Addr),Ein)
	->  Tout = Tin, Eout = Ein
	;   Tout is Tin+4, Eout = [(V->Tin)|Ein], Addr = Tin),
	write('    ecx = *(int*)&M['),
	write(Addr),
	write('] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push '),
	writeln(V).

compileExpr(Exp,Ein,Eout,Tin,Tout) :-
	Exp =.. [O,A,B],
	compileExpr(A,Ein,Eaux,Tin,Taux),
	compileExpr(B,Eaux,Eout,Taux,Tout),
	writeln('    ecx = *(int*)&M[esp] ; esp += 4 ;'),
	writeln('    eax = *(int*)&M[esp] ; esp += 4 ;'),
	write('    eax '), write(O), writeln('= ecx ;'),
	write('    esp -= 4 ; *(int*)&M[esp] = eax ; // push result of '),
	writeln(O).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ARRAY 
compile(A@K=E,Ein,Eout,Tin,Tout,L,L) :-
	compileExpr(E,Ein,Eaux,Tin,Taux),				% compiled expression
	compileExpr(A@K,Eaux,Eout,Taux,Tout).			% initialized space for array
		
compile(V=A@K,Ein,Eout,Tin,Tout,L,L) :-
	compileExpr(A@K,Ein,Eaux,Tin,Taux),
	(   member((V->Addr),Eaux)
	->  Tout = Taux, Eout = Eaux
	;   Tout is Taux+4, Eout = [(V->Taux)|Eaux], Addr = Taux),
	writeln('    ecx = *(int*)&M[esp] ; esp += 4 ;'),
	write('    *(int*)&M['),write(Addr),write('] = ecx ; // pop '),
	writeln(V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile(V=E,Ein,Eout,Tin,Tout,L,L) :-
	compileExpr(E,Ein,Eaux,Tin,Taux),
	(   member((V->Addr),Eaux)
	->  Tout = Taux, Eout = Eaux
	;   Tout is Taux+4, Eout = [(V->Taux)|Eaux], Addr = Taux),
	writeln('    ecx = *(int*)&M[esp] ; esp += 4 ;'),
	write('    *(int*)&M['),write(Addr),write('] = ecx ; // pop '),
	writeln(V).

% if then else
compile(if B then S1 else S2,Ein,Eout,Tin,Tout,Lin,Lout) :- !,
	B =.. [O,X,Y], La1 is Lin+1,
	(   O == (\=) -> Otrans = '!=' ; Otrans = O ),
	writeln('    // start of if-then-else statement'),
	compileExpr(X,Ein,Ea1,Tin,Ta1),
	compileExpr(Y,Ea1,Ea2,Ta1,Ta2),
	writeln('    ecx = *(int*)&M[esp] ; esp += 4 ;') ,
	writeln('    eax = *(int*)&M[esp] ; esp += 4 ;') ,
	write('    if ( eax '), write(Otrans),
	write(' ecx ) goto Lthen'), write(Lin), writeln('; // if condition'),
	compile(S2,Ea2,Ea3,Ta2,Ta3,La1,La2),
	write('    goto Lendif'),write(Lin),writeln(';'),
	write('Lthen'),write(Lin),writeln(':'),
	compile(S1,Ea3,Eout,Ta3,Tout,La2,Lout),
	write('Lendif'),write(Lin),writeln(':').


% if ... then	
compile(if B then S,Ein,Eout,Tin,Tout,Lin,Lout) :- !,
	B =.. [O,X,Y], La1 is Lin+1,
	(   O == (\=) -> Otrans = '!=' ; Otrans = O ),
	writeln('    // start of if-then statement'),
	compileExpr(X,Ein,Ea1,Tin,Ta1),
	compileExpr(Y,Ea1,Ea2,Ta1,Ta2),
	writeln('    ecx = *(int*)&M[esp] ; esp += 4 ;') ,
	writeln('    eax = *(int*)&M[esp] ; esp += 4 ;') ,
	write('    if ( eax '), write(Otrans),
	write(' ecx ) goto Lthen'), write(Lin), writeln('; // if condition'),
	write('    goto Lendif'),write(Lin),writeln(';'),
	write('Lthen'),write(Lin),writeln(':'),
	compile(S,Ea2,Eout,Ta2,Tout,La1,Lout),
	write('Lendif'),write(Lin),writeln(':').

% while-do loop	
compile(while B do S,Ein,Eout,Tin,Tout,Lin,Lout) :- !,
	B =.. [O,X,Y], La1 is Lin+1,
	(   O == (\=) -> Otrans = '!=' ; Otrans = O ),
	write('Lwhile'),write(Lin),writeln(':'),
	compileExpr(X,Ein,Ea1,Tin,Ta1),
	compileExpr(Y,Ea1,Ea2,Ta1,Ta2),
	writeln('    ecx = *(int*)&M[esp] ; esp += 4 ;') ,
	writeln('    eax = *(int*)&M[esp] ; esp += 4 ;') ,
	write('    if ( eax '), write(Otrans),
	write(' ecx ) goto Lwhilebody'), write(Lin), writeln(';'),
	write('    goto Lendwhile'),write(Lin),writeln(';'),
	write('Lwhilebody'),write(Lin),writeln(':'),
	compile(S,Ea2,Eout,Ta2,Tout,La1,Lout),
	write('    goto Lwhile'),write(Lin),writeln(';'),
	write('Lendwhile'),write(Lin),writeln(':').




compile(S1;S2,Ein,Eout,Tin,Tout,Lin,Lout) :- !,
	compile(S1,Ein,Eaux,Tin,Taux,Lin,Laux),
	compile(S2,Eaux,Eout,Taux,Tout,Laux,Lout).
compile(S;,Ein,Eout,Tin,Tout,Lin,Lout) :- !,
	compile(S,Ein,Eout,Tin,Tout,Lin,Lout).
compile({S},Ein,Eout,Tin,Tout,Lin,Lout) :- !,
	compile(S,Ein,Eout,Tin,Tout,Lin,Lout).


compileProg(P) :-
	writeln('#include <stdio.h>'),
	writeln('int eax,ebx,ecx,edx,esi,edi,ebp,esp;'),
	writeln('unsigned char M[10000];'),
	writeln('void exec(void) {'),
	compile(P,[],Eout,0,_,0,_),
	writeln('{}}'),nl,
	writeln('int main() {'),
	writeln('    esp = 10000 ;'),
	writeln('    exec();'),
	outputVars(Eout),
	writeln('    return 0;'),
	writeln('}').

outputVars([]).
outputVars([(V->Addr)|T]) :-
	write('    printf("'),write(V),write('=%d\\n",'),
	write('*(int*)&M['),write(Addr),writeln(']);'),
	outputVars(T).

:- P = (	
   			a@0 = 10 ;
   			a@1 = 40 ;
   			a@2 = 30 ;
   			m = 20 ;
			i = 0 ;
			while ( i < 3 ) do {
  				if ( m < a@i ) then { m = a@i ; } ;
  				i = i + 1 ;
			} ;
       ),
	compileProg(P).
