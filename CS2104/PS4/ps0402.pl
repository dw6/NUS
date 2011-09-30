% Benjamin Tan Wei Hao
% U077129N
% Problem Set 4 : Exercise 2

:- op(1099,yf,;).
:- op(960,fx,if).
:- op(959,xfx,then).
:- op(958,xfx,else).
:- op(960,fx,while).
:- op(959,xfx,do).

% Integers
compileExpr(K,E,E,T,T) :-
	integer(K),!,
	write('    esp -= 4 ; *(int*)&M[esp] = '),
	write(K),write(' ; // push '), writeln(K).

% Variables
compileExpr(V,Ein,Eout,Tin,Tout) :-
	atom(V),!,
	(   member((V->Addr),Ein)
	->  Tout = Tin, Eout = Ein
	;   Tout is Tin+4, Eout = [(V->Tin)|Ein], Addr = Tin),
	write('    ecx = *(int*)&M['),
	write(Addr),
	write('] ; esp -= 4 ; *(int*)&M[esp] = ecx ; // push '),
	writeln(V).

% Other Expression
compileExpr(Exp,Ein,Eout,Tin,Tout) :-
	Exp =.. [O,A,B],
	compileExpr(A,Ein,Eaux,Tin,Taux),
	compileExpr(B,Eaux,Eout,Taux,Tout),
	writeln('    ecx = *(int*)&M[esp] ; esp += 4 ;'),
	writeln('    eax = *(int*)&M[esp] ; esp += 4 ;'),
	write('    eax '), write(O), writeln('= ecx ;'),
	write('    esp -= 4 ; *(int*)&M[esp] = eax ; // push result of '),
	writeln(O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% An even more awesome way of performing assignments

compile((V,W)=(E,F),Ein,Eout,Tin,Tout,L,L) :-
	% Compilation of expressions must occur first
	compileExpr(E,Ein,Eaux1,Tin,Taux1),
	compileExpr(F,Eaux1,Eaux2,Taux1,Taux2),
	% Then handle assignments
	(   member((W->Addr1),Eaux2)
	->  Taux3 = Taux2, Eaux3 = Eaux2
	;   Taux3 is Taux2+4, Eaux3 = [(W->Taux2)|Eaux2], Addr1 = Taux2),
	writeln('    ecx = *(int*)&M[esp] ; esp += 4 ;'),
	write('    *(int*)&M['),write(Addr1),write('] = ecx ; // pop '),
	writeln(W),
	(   member((V->Addr2),Eaux2)
	->  Tout = Taux3, Eout = Eaux3
	;   Tout is Taux3+4, Eout = [(V->Taux3)|Eaux3], Addr2 = Taux3),
	writeln('    ecx = *(int*)&M[esp] ; esp += 4 ;'),
	write('    *(int*)&M['),write(Addr2),write('] = ecx ; // pop '),
	writeln(V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Assignments
compile(V=E,Ein,Eout,Tin,Tout,L,L) :-
	compileExpr(E,Ein,Eaux,Tin,Taux),
	(   member((V->Addr),Eaux)
	->  Tout = Taux, Eout = Eaux
	;   Tout is Taux+4, Eout = [(V->Taux)|Eaux], Addr = Taux),
	writeln('    ecx = *(int*)&M[esp] ; esp += 4 ;'),
	write('    *(int*)&M['),write(Addr),write('] = ecx ; // pop '),
	writeln(V).


% if ... then .. else	
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

% while ... do 	
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

% These examples work ! :)

% :- P = (
%           % Test Case 1
%           x = 3 ;
%           y = 4 ;
%           (x,y) = (y,x) ;

%           % Test Case 2
%           % (x,y) = (3+4,5+3) ;

%           % Test Case 3
%           % x = 3 ;
%           % y = 4 ;
%           % (x,y) = (x+y,y-x) ;
%        ),
% 	compileProg(P).

:- P = (

          (x,y) = (144,60) ;

          while ( x \= y ) do {
             if ( x < y ) then {
                y = y - x ;
             } else {
                x = x - y ;
             } ;
          } ;
       ),
	compileProg(P).
