%  ---=== [ Benjamin Tan presents ....] ====---
%
%       /\__  _\/\  _  \/\  _`\  /\ \        
%   ____\/_/\ \/\ \ \L\ \ \ \/\_\\ \ \/'\    
%  /',__\  \ \ \ \ \  __ \ \ \/_/_\ \ , <    
% /\__, `\  \ \ \ \ \ \/\ \ \ \L\ \\ \ \\`\  
% \/\____/   \ \_\ \ \_\ \_\ \____/ \ \_\ \_\
%  \/___/     \/_/  \/_/\/_/\/___/   \/_/\/_/
%
% ... because TAC is too complicated. :)
% ==============================================

:- op(950,fx,goto).
:- op(950,fx,push).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % Operational semantics of object code %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getHeap(E,Env,Heap,Addr,Val) :-
	evalTaiRhs(E,Env,Addr),
	(   get_assoc(Addr,Heap,Val)
	->  true
	;   Val = undef
	).


% 'push' instruction : 
%	- pushes an element on top of the stack
%	- environment and heap does not change
execSi(push X,Env,Heap,Env,Heap,StackIn,StackOut) :- 
	append([X],StackIn,StackOut), !.


% 'pop' instruction : 
%	- removes the top-most element from the stack
%	- environment and heap does not change
execSi(pop,Env,Heap,Env,Heap,[_H|T],[T]) :- !.

% 'store' instruction :
% 	- first  pop : address of the variable
%   - second pop : value to assign to the variable
execSi(store,Env,Heap,Env,NewHeap,StackIn,StackOut) :- 
	append([Addr],StackAux,StackIn),
	append([Value],StackOut,StackAux),
	put_assoc(Addr,Heap,Value,NewHeap), !.

% 'load' instruction :
% 	- pop : address of the variable to load	
% 	- push : the value of the variable pointed by the address
execSi(load,Env,Heap,Env,Heap,StackIn,StackOut) :- 
	append([Addr],StackAux,StackIn),
	get_assoc(Addr,Heap,Value),
	append([Value],StackAux,StackOut).


% operators :
%	- pop B, pop A, perform operation, push in result
execSi(F,Env,Heap,Env,Heap,StackIn,StackOut) :- 
	append([B],StackAux,StackIn),
	append([A],StackAux2,StackAux),
	member(F,[+,-,*,/,mod,<<,>>,/\,\/,and,or,xor]),	
	Op =.. [F,A,B], Val is Op,
	append([Val],StackAux2,StackOut).


execSi(F,Env,Heap,Env,Heap,StackIn,StackOut) :-
	member(F,[<,>,=<,>=,==,\=]),	
	append([B],StackAux,StackIn),
	append([A],StackAux2,StackAux),
	Op =.. [F,A,B], 
	(Op -> Result = 1 ; Result = 0),
	append([Result],StackAux2,StackOut).


% 'select' instruction :
%	- pop 3 times
% 	- 1 : Not Zero ? Push 2 : Push 3
execSi(sel,Env,Heap,Env,Heap,StackIn,StackOut) :- 
	append([A],StackAux,StackIn),
	append([B],StackAux2,StackAux),
	append([C],StackAux3,StackAux2),
	(	A = 0 -> 
			append([C],StackAux3,StackOut); 
			append([B],StackAux3,StackOut) 
 	).

% :- dynamic counter/1.
% :- retractall(counter(_)).

% counter(0).

execObj(IP,Code,Env,Heap,Env,Heap,Stack,Stack) :- max_assoc(Code,K,_), IP > K, !.

execObj(IP,Code,EnvIn,HeapIn,EnvOut,HeapOut,StackIn,StackOut) :-
        get_assoc(IP,Code,Instr),
        (   
        	Instr = jmp, !, 
        	append([IPnext],StackAux,StackIn), 
			EnvAux = EnvIn, HeapAux = HeapIn
        ;   
        	Instr = cjmp, !, 
        	append([Value],StackAux1,StackIn), append([Addr],StackAux,StackAux1),
        	(Value is 0 -> IPnext = Addr; IPnext is IP+1),
			EnvAux = EnvIn, HeapAux = HeapIn
		;	
        	execSi(Instr,EnvIn,HeapIn,EnvAux,HeapAux,StackIn,StackAux), IPnext is IP+1 
        ),
        execObj(IPnext,Code,EnvAux,HeapAux,EnvOut,HeapOut,StackAux,StackOut).

/********************************************
 * Compiler from HL language to TO S|TAC|K
 ********************************************/

:- dynamic auxreg/1.
:- dynamic auxvar/1.

resetnewreg :-
        (   retractall(auxreg(_)) -> true ; true ),
        assert(auxreg(0)).

% generate new label names - same as 02.pl

newlabel(X) :-
        retract(auxlbl(Y))
        ->  Y1 is Y+1, assert(auxlbl(Y1)), atomic_concat('l_',Y1,X)
        ;   assert(auxlbl(0)), X = 'l_0' .

resetnewlabel :-
        (   retractall(auxlbl(_)) -> true ; true ),
        assert(auxlbl(0)).

% allocate new addresses for program variables
% TopIn/TopOut record the top of heap before/after allocation
newvars(X,EnvIn,TopIn,EnvOut,TopOut) :-
	atom(X), !, TopOut is TopIn+4,
	createEnv(X,EnvIn,EnvAux), putEnv(X,EnvAux,TopIn,EnvOut).

newvars((V,Vs),EnvIn,TopIn,EnvOut,TopOut) :-
	newvars(V,EnvIn,TopIn,EnvAux,TopAux),
	newvars(Vs,EnvAux,TopAux,EnvOut,TopOut).

% X is a constant integer :- Push into the stack
compileExpr(X,push X,X,_Env) :- integer(X), !.

% X is a variable :- Load it's address into the stack	
compileExpr(X, push Addr ; load ,Addr,Env) :- atom(X), !, getEnv(X,Env,Addr).


compileExpr(E,Code,_Result,Env) :-
	% Binary Operators.
	% First push both operands into the stack
	E =.. [F,A,B], member(F,[+,-,*,/,mod,<<,>>,/\,\/,and,or,xor,<,>,=<,>=,==,\=]), !,
	compileExpr(A,CodeA,_RA,Env), 
	compileExpr(B,CodeB,_RB,Env),
	Code = (CodeA ; CodeB; F).

% -----------------------------

compileExpr(E,Code,R,Env) :-
	E =.. [F,A], member(F,[+,-]), !,
	C =.. [F,0,A], compileExpr(C,Code,R,Env).

compileExpr(\ X,Code,R,Env) :- !,
	compileExpr((-1) xor X, Code, R, Env).

compileStmt((int L),nop,EnvIn,TopIn,EnvOut,TopOut) :-
	newvars(L,EnvIn,TopIn,EnvOut,TopOut).

compileStmt((X=E),(CE;C),Env,Top,Env,Top) :-
	compileExpr(E,CE,_R,Env),
	(   getEnv(X,Env,Addr)
	->  true
	;   write('Undeclared variable: '),
	    writeln(X), abort ),
	C1 = (store),
	C = ( 
		push Addr ;
		C1
	).

compileStmt((if B then {S1} else {S2}),Code,Env,Top,Env,Top) :-
	newlabel(LblS1), newlabel(LblS2), newlabel(LblOut), 
	compileExpr(B,CB,_RB,Env), 		
	compileStmt({S1},CS1,Env,Top,_,_), 
	compileStmt({S2},CS2,Env,Top,_,_),
	Code = (push LblS2 ; push LblS1 ;
		    CB ; sel ; jmp ; 
		  	LblS2:: ; CS2 ; push LblOut ; jmp ;
		  	LblS1:: ; CS1 ; push LblOut ; jmp ;
		  	LblOut::).

compileStmt((if B then S),Code,Env,Top,Env,Top) :- !,
        newlabel(Lout),
        compileExpr(B,CodeB,_R,Env),
       	compileStmt(S,CodeS,Env,Top,_,_),  
       	Code = ( push Lout ; CodeB ; cjmp ; CodeS ; Lout:: ).

compileStmt((while B do { S }),Code,Env,Top,Env,Top) :-
	newlabel(LblTop), newlabel(LblOut),
	compileExpr(B,CB,_RB,Env),
	compileStmt({S},CS,Env,Top,_,_),
    Code = (LblTop::; push LblOut ; CB ; cjmp ; CS ; push LblTop ; jmp ; LblOut:: ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fibonacci
%
% This algorithm calculates the factorial of 'n'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- Tac = (
			nop     ;
            push 1  ;
            push 0  ; 
            store   ;
            push 1  ;
            push 4  ;	
            store   ;
            push 3  ;
            push 12 ;
            store   ;
            push 10 ;		% Change 'n' in fibonacci(n) here
            push 16 ;
            store   ;
			l_2::	;
            push l_3;
            push 12 ;
            load    ;
            push 16 ;
            load    ;
            =<      ;
            cjmp    ;
            push 0  ;
            load    ;
            push 4  ;
            load    ;
            +       ;
            push 8  ;
            store   ;
            push 4  ;
            load    ;
            push 0  ;
            store   ;
            push 8  ;
            load    ;
            push 4  ;
            store   ;
            push 12 ;
            load    ;
            push 1  ;
            +       ;
            push 12 ;
            store   ;
            push l_2;
            jmp     ;
			l_3:: 	  		
		 ).

