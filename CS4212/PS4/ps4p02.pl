% Benjamin Tan Wei Hao
% U077129N
% Problem Set 4 Ex 2

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
                                           

% All operator declarations at the top
%
:- op(800,yfx,and).
:- op(810,yfx,or).
:- op(1099,yf,;).
:- op(960,fx,if).
:- op(959,xfx,then).
:- op(958,xfx,else).
:- op(960,fx,while).
:- op(959,xfx,do).
:- op(1090,fx,int).
:- op(950,fx,goto).
:- op(950,fx,push).
:- op(950,xfx,goto).
:- op(970,xfy,::).
:- op(969,xf,::).

% For convenience, we add 'and' and 'or' to 'is'.
% Try help(arithmetic_function) at Prolog prompt for full docs.
and(X,Y,Val) :- Val is abs(sign(X)) /\ abs(sign(Y)).
or(X,Y,Val) :- Val is abs(sign(X)) \/ abs(sign(Y)).
:- arithmetic_function(and/2).
:- arithmetic_function(or/2).

% Environments must be enhanced to handle multiple scopes
% An environment is a stack of association lists ; stacks can
% be implemented as lists.
%
% It is convenient to have a full API for environments

% get the value of a variable from the environment,
%	-- fail if var does not exist

getEnv(X,[],_) :- !,
	write('Attempt to retrieve undefined identifier: '), writeln(X), abort.
getEnv(X,[Top|_],Val) :- get_assoc(X,Top,Val),!.
getEnv(X,[_|Rest],Val) :- getEnv(X,Rest,Val).

% put a value for a var in an environment
%   -- fail if var does not exist (not declaring vars is an error now)

putEnv(X,[],_,_) :- !,
	write('Attempt to assign undefined identifier: '), writeln(X), abort.
putEnv(X,[Top|Rest],Val,[NewTop|Rest]) :-
	get_assoc(X,Top,_),!,
	put_assoc(X,Top,Val,NewTop).
putEnv(X,[Top|Rest],Val,[Top|NewRest]) :-
	putEnv(X,Rest,Val,NewRest).

% create a new variable in current env. Initial value is 'undef'
%   -- makes it possible to track uninitialized variables in the
%      interpreter

createEnv(X,[Top|Rest],[NewTop|Rest]) :-
	(   get_assoc(X,Top,_)
	->  write('Duplicate name in environment: '),
	    writeln(X), abort
	;   put_assoc(X,Top,undef,NewTop) ).

% expand an environment ; used when entering a new scope
%   -- the expansion is an association list added to the top of stack
%   -- the expansion can hold vars with same names as ones in enclosing
%      scopes without conflict.

expandEnv(Env,[Empty|Env]) :- empty_assoc(Empty).

% shrink an environment: useful when exiting a scope

shrinkEnv([_|Env],Env).


evalExpr(X,Env,Val) :-
	atom(X),!, \+ atom_prefix(X,v_), getEnv(X,Env,Val),
	(   Val = undef
	->  write('Variable '), write(X), writeln(' unitialized.'), abort
	;   true ).
evalExpr(X,_,X) :- integer(X),!.
evalExpr(E,Env,Val) :-
	E =.. [F,A,B],
	member(F,[+,-,*,/,mod,and,or,/\,\/,<<,>>,xor]), !,
	evalExpr(A,Env,ValA), evalExpr(B,Env,ValB),
	(   F == / -> C = ValA // ValB ; C =.. [F,ValA,ValB] ),
	Val is C.
evalExpr(E,Env,Val) :-
	E =.. [F,A,B],
	member(F,[<,>,=<,>=,==,\= ]), !,
	evalExpr(A,Env,ValA), evalExpr(B,Env,ValB),
	C =.. [F,ValA,ValB],
	(   C -> Val = 1 ; Val = 0 ).
evalExpr(E,Env,Val) :-
	E =.. [F,A],
	member(F,[+,-,\]),!,
	evalExpr(A,Env,ValA),
	C =.. [F,ValA], Val is C.
evalExpr((X ? Y : Z), Env, Val) :-
	evalExpr(X,Env,ValA),
	(   ValA \= 0
	->  evalExpr(Y,Env,Val)
	;   evalExpr(Z,Env,Val) ).

% helper predicate to handle a list of variables in a declaration
% each variable in the list is added to the current environment

createVars( (X,Y), EnvIn, EnvOut ) :- !,
	createVars(X,EnvIn,EnvAux), createVars(Y,EnvAux,EnvOut).
createVars( X, EnvIn, EnvOut ) :-
	(   atom(X), \+ atom_prefix(X,v_)
	->  createEnv(X,EnvIn,EnvOut)
	;   write('Incorrect variable name:'), writeln(X), abort ).

% Statement execution predicate, has a new feature: variable
% declarations
%    -- the fourth argument is useful in handling 'while' loops
%    -- when entering 'while' loop body a second time, all declarations
%       must be disregarded
%    -- Fourth argument possible values:
%	  'firstTime' : used when a stmt is executed for the first time
%	  'secondTime': used when a stmt is subsequently executed again

execStmt((int L),EnvIn,EnvOut,Flag) :- !,
	(   Flag == firstTime
	->  createVars(L,EnvIn,EnvOut)
	;   EnvOut = EnvIn ).
execStmt((X=E),EnvIn,EnvOut,_) :- !,
	evalExpr(E,EnvIn,Val), putEnv(X,EnvIn,Val,EnvOut).
execStmt((while B do { S }),EnvIn,EnvOut,firstTime) :-!,
	 evalExpr(B,EnvIn,Val),
	 (   Val \= 0
	 ->  expandEnv(EnvIn,Env1),
	     execHL(S,Env1,Env2,firstTime),
	     execStmt((while B do { S }), Env2, Env3, secondTime),
	     shrinkEnv(Env3,EnvOut)
	 ;   EnvOut = EnvIn ).
execStmt((while B do { S }),EnvIn,EnvOut,secondTime) :-!,
	 shrinkEnv(EnvIn,EnvB),
	 evalExpr(B,EnvB,Val),
	 (   Val \= 0
	 ->  execHL(S,EnvIn,Env2,secondTime),
	     execStmt((while B do { S }), Env2, EnvOut, secondTime)
	 ;   EnvOut = EnvIn).
execStmt((if B then { S1 } else { S2 }),EnvIn,EnvOut,_) :- !,
        evalExpr(B,EnvIn,Val),
	expandEnv(EnvIn,Env1),
        (   Val \= 0
        ->  execHL(S1,Env1,Env2,firstTime)
        ;   execHL(S2,Env1,Env2,firstTime) ),
	shrinkEnv(Env2,EnvOut).
execStmt((if B then { S } ),EnvIn,EnvOut,_) :- !,
        evalExpr(B,EnvIn,Val),
	expandEnv(EnvIn,Env1),
        (   Val \= 0
        ->  execHL(S,Env1,Env2,firstTime)
        ;   Env1 = Env2 ),
	shrinkEnv(Env2,EnvOut).
execStmt({S},EnvIn,EnvOut,_) :-
	expandEnv(EnvIn,Env1),
	execHL(S,Env1,Env2,firstTime),
	shrinkEnv(Env2,EnvOut).

execHL((S1;S2),EnvIn,EnvOut,Flag) :- !,
	execStmt(S1,EnvIn,EnvAux,Flag), execHL(S2,EnvAux,EnvOut,Flag).
execHL((S;),EnvIn,EnvOut,Flag) :- !,
	execStmt(S,EnvIn,EnvOut,Flag).

% Test high level interpreter

% Three address code and object code
%   -- enhanced to handle references
%   -- new instructions: [Addr] = reg ; reg = [Addr]
%        where Addr is an address expression
%   -- contents of address Addr is transferred to/from register
%   -- memory (or "heap", in most PL) is modelled as association list
%          with mappings Address -> Value
%          - addresses are multiples of 4 for holding integer values
%
% There is no syntax checker, but the predicates only accept correct
% TAC/OBJ code.
%
% Main predicate is tacToObj(TacCode,ObjCode).

% Three address instruction with no label --> Object instruction
taiNLtoObj(push X,P,P,push X) :- integer(X),!.
taiNLtoObj(push X,P,[X:Z|P],push Z) :- atom(X), !. % Map 'push lbl' --> 'push addr'
taiNLtoObj(I,P,P,I) :- member(I, [nop,pop,store,jmp,cjmp,sel,load]), !.
taiNLtoObj(Op,P,P,Op) :- !, member(Op,[+,-,*,/,mod,/\,\/,xor,<<,>>,<,>,=<,>=,==,\=]).

% Three address instruction (possibly labelled) --> Object instruction
%    -- add pair (Label,IP) to symbol table

taiToObj(L::nop,IP,Lin,Lout,Pin,Pout,T) :- taiToObj(L::,IP,Lin,Lout,Pin,Pout,T).

taiToObj(L::I,IP,Lin,Lout,Pin,Pout,T) :- !,
        get_assoc(L,Lin,_)
        ->  writeln('Duplicate labels'), abort
        ;   put_assoc(L,Lin,IP,Laux),
            (   I =.. [(::)|_]
             -> taiToObj(I,IP,Laux,Lout,Pin,Pout,T
             ;  Lout=Laux, taiNLtoObj(I,Pin,Pout,T))) .

taiToObj((L::),IP,Lin,Lout,P,P,none) :-
        get_assoc(L,Lin,_)
        -> writeln('Duplicate labels'), abort
        ;  put_assoc(L,Lin,IP,Lout).


taiToObj(I,_,L,L,Pin,Pout,T) :- taiNLtoObj(I,Pin,Pout,T).

% % First pass: translate instructions, accumulate labels and
% % corresponding IPs in symbol table, and create placeholders
% % for addresses in object code
firstPass((C;nop),IPin,IPout,Lin,Lout,Pin,Pout,Tin,Tout) :- !,
	firstPass(C,IPin,IPout,Lin,Lout,Pin,Pout,Tin,Tout).

firstPass((nop;C),IPin,IPout,Lin,Lout,Pin,Pout,Tin,Tout) :- !,
	firstPass(C,IPin,IPout,Lin,Lout,Pin,Pout,Tin,Tout).

firstPass((C1;C2),IPin,IPout,Lin,Lout,Pin,Pout,Tin,Tout) :- !,
        firstPass(C1,IPin,IPaux,Lin,Laux,Pin,Paux,Tin,Taux),
        firstPass(C2,IPaux,IPout,Laux,Lout,Paux,Pout,Taux,Tout).

firstPass((I;),IPin,IPout,Lin,Lout,Pin,Pout,Tin,Tout) :- !,
        taiToObj(I,IPin,Lin,Lout,Pin,Pout,T),
        put_assoc(IPin,Tin,T,Tout),
        IPout is IPin + 1.
        
firstPass(I,IPin,IPout,Lin,Lout,Pin,Pout,Tin,Tout) :-
        taiToObj(I,IPin,Lin,Lout,Pin,Pout,T),
        ( T = none
          -> IPout = IPin, Tout = Tin
          ; put_assoc(IPin,Tin,T,Tout), IPout is IPin + 1 ).

% % Second pass: replace the placeholders in code by addresses that
% % were computed for labels.
secondPass([],_).
secondPass([Lbl:P|T],L) :-
        (   get_assoc(Lbl,L,P)
	->  true
        ;   P = Lbl ),
	secondPass(T,L).

% % Main predicate, applies the two passes to a TAC
tacToObj(SrcCode,ObjCode) :-
        empty_assoc(Empty),
        firstPass(SrcCode,0,_,Empty,Labels,[],Plholders,Empty,ObjCode),
        secondPass(Plholders,Labels).

% % Pretty-printing of TAC
alignLabel(X,Y) :-
        atomic_concat(X,'          ',Z),
        atom_chars(Z,L), append(L1,_,L), length(L1,10),
        atomic_list_concat(L1,Y).

writeTac((X;Y)) :- !, writeTac(X), writeTac(Y).
writeTac((X::Y::Z)) :- !, writeTac((X::);(Y::Z)).
writeTac((X::Y)) :- !, alignLabel(X,X1),writeln((X1::Y)).
writeTac((X::)) :- !, alignLabel(X,X1), write(X1), writeln((::)).
writeTac(X) :- write('            '),writeln(X).

% % pretty-printing of Obj
alignIP(X,Y) :-
        atomic_concat('   ',X,Z),
        atom_chars(Z,L), append(_,L1,L), length(L1,3),
        atomic_list_concat(L1,Y).

writeObj(Obj,IP) :- max_assoc(Obj,K,_), IP > K, !.
writeObj(Obj,IP) :-
        get_assoc(IP,Obj,I), alignIP(IP,X),write(X), write(' :: '), writeln(I),
        IPnext is IP + 1, writeObj(Obj,IPnext).

% % Operational semantics of object code 

getHeap(E,Env,Heap,Addr,Val) :-
	evalTaiRhs(E,Env,Addr),
	(   get_assoc(Addr,Heap,Val)
	->  true
	;   Val = undef
	).


% 'push' instruction : 
%	- pushes an element on top of the stack
%	- environment and heap does not change
execTai(push X,Env,Heap,Env,Heap,StackIn,StackOut) :- 
	append([X],StackIn,StackOut), !.


% 'pop' instruction : 
%	- removes the top-most element from the stack
%	- environment and heap does not change
execTai(pop,Env,Heap,Env,Heap,[_H|T],[T]) :- !.

% 'store' instruction :
% 	- first  pop : address of the variable
%   - second pop : value to assign to the variable
execTai(store,Env,Heap,Env,NewHeap,StackIn,StackOut) :- 
	append([Addr],StackAux,StackIn),
	append([Value],StackOut,StackAux),
	put_assoc(Addr,Heap,Value,NewHeap), !.

% 'load' instruction :
% 	- pop : address of the variable to load	
% 	- push : the value of the variable pointed by the address
execTai(load,Env,Heap,Env,Heap,StackIn,StackOut) :- 
	append([Addr],StackAux,StackIn),
	get_assoc(Addr,Heap,Value),
	append([Value],StackAux,StackOut).


% operators :
%	- pop B, pop A, perform operation, push in result
execTai(F,Env,Heap,Env,Heap,StackIn,StackOut) :- 
	append([B],StackAux,StackIn),
	append([A],StackAux2,StackAux),
	member(F,[+,-,*,/,mod,<<,>>,/\,\/,and,or,xor]),	
	Op =.. [F,A,B], Val is Op,
	append([Val],StackAux2,StackOut).


execTai(F,Env,Heap,Env,Heap,StackIn,StackOut) :-
	member(F,[<,>,=<,>=,==,\=]),	
	append([B],StackAux,StackIn),
	append([A],StackAux2,StackAux),
	Op =.. [F,A,B], 
	(Op -> Result = 1 ; Result = 0),
	append([Result],StackAux2,StackOut).


% 'select' instruction :
%	- pop 3 times
% 	- 1 : Not Zero ? Push 2 : Push 3
execTai(sel,Env,Heap,Env,Heap,StackIn,StackOut) :- 
	append([A],StackAux,StackIn),
	append([B],StackAux2,StackAux),
	append([C],StackAux3,StackAux2),
	(	A = 0 -> 
			append([C],StackAux3,StackOut); 
			append([B],StackAux3,StackOut) 
 	).


	

%%%%%%%%%%% <----------- Progress  -----------> %%%%%%%%%%%

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
        	execTai(Instr,EnvIn,HeapIn,EnvAux,HeapAux,StackIn,StackAux), IPnext is IP+1 
        ),
        execObj(IPnext,Code,EnvAux,HeapAux,EnvOut,HeapOut,StackAux,StackOut).

/********************************************
 * Compiler from HL language to TO S|TAC|K
 ********************************************/

:- dynamic auxreg/1.
:- dynamic auxvar/1.

% generate new register names - same as 02.pl, but renamed predicate
%  ( the variables of 02.pl are now called registers; variables are
%    stored in memory, and are allocated addresses )
% newreg(X) :- atom(X),!.
% newreg(X) :-
%         retract(auxreg(Y))
%         ->  Y1 is Y+1, assert(auxreg(Y1)), atomic_concat('r_',Y1,X)
%         ;   assert(auxreg(0)), X = 'r_0' .

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
compileExpr(X,push X,X,_Env) :-
	integer(X), !.

% X is a variable :- Load it's address into the stack	
compileExpr(X, push Addr ; load ,Addr,Env) :-
	atom(X), !, getEnv(X,Env,Addr).


compileExpr(E,Code,Result,Env) :-
	% Binary Operators.
	% First push both operands into the stack
	E =.. [F,A,B], member(F,[+,-,*,/,mod,<<,>>,/\,\/,and,or,xor,<,>,=<,>=,==,\=]), !,
	compileExpr(A,CodeA,RA,Env), write('A: '), writeln(CodeA),	% push RA
	compileExpr(B,CodeB,RB,Env), write('B: '), writeln(CodeB),   % push RB
	Code = (CodeA ; CodeB; F).


% -----------------------------

% compileExpr((X ? Y : Z),Code,R,Env) :- !,
%         newreg(R), newlabel(Skip), newlabel(Lout),
%         compileExpr(X,Cx,Qx,Env),
%         compileExpr(Y,Cy,Qy,Env),
%         compileExpr(Z,Cz,Qz,Env),
% 	C1 = (if Qx == 0 goto Skip),
%         C2 = (R = Qy ; goto Lout),
%         C3 =  (R = Qz),
%         Code = (Cx ; C1 ; Cy ; C2 ; Skip::; Cz; C3; Lout::).

compileExpr(E,Code,R,Env) :-
	E =.. [F,A], member(F,[+,-]), !,
	C =.. [F,0,A], compileExpr(C,Code,R,Env).

compileExpr(\ X,Code,R,Env) :- !,
	compileExpr((-1) xor X, Code, R, Env).

% Compilation of statements
% Here, we add slight optimization, to avoid instruction of the form
%   Reg = Const ; as a result, some HL statements may generate no code
% -- declarations may add variables to environment; thus each rule has
%    input environment and a output environment
% -- for easy address allocation, the top free address in an
%    environment is passed in and out
% -- label requests are still needed; same use as for compileExpr

% Declarations [Only int]
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
	compileExpr(B,CB,RB,Env), 		
	compileStmt({S1},CS1,Env,Top,_,_), 
	compileStmt({S2},CS2,Env,Top,_,_),
	Code = (push LblS2 ; push LblS1 ;
		    CB ; sel ; jmp ; 
		  	LblS2:: ; CS2 ; push LblOut ; jmp ;
		  	LblS1:: ; CS1 ; push LblOut ; jmp ;
		  	LblOut::).


compileStmt((if B then S),Code,Env,Top,Env,Top) :- !,
        newlabel(Lout),
        compileExpr(B,CodeB,R,Env),
       	compileStmt(S,CodeS,Env,Top,_,_),  
       	Code = ( push Lout ; CodeB ; cjmp ; CodeS ; Lout:: ).

compileStmt((while B do { S }),Code,Env,Top,Env,Top) :-
	newlabel(LblTop), newlabel(LblOut),
	compileExpr(B,CB,RB,Env),
	compileStmt({S},CS,Env,Top,_,_),
    Code = (LblTop::; push LblOut ; CB ; cjmp ; CS ; push LblTop ; jmp ; LblOut:: ).


compileStmt({S},Code,Env,Top,Env,Top) :-
	compileHL(S,Code,Env,Top,_,_).

% Top level compilation predicate. It accepts only programs
% with correct syntax

compileHL((S1;S2),(C1;C2),Env,Top,NewEnv,NewTop) :-
	compileStmt(S1,C1,Env,Top,AuxEnv,AuxTop),
	compileHL(S2,C2,AuxEnv,AuxTop,NewEnv,NewTop).
compileHL((S;),Code,Env,Top,NewEnv,NewTop) :-
	compileStmt(S,Code,Env,Top,NewEnv,NewTop).

% Compiler test
% :- resetnewreg, resetnewlabel.

% :- Program = (
% 				% Factorial
% 				int y ;
% 				y = 1 ;
%     			int x ;
%     			x = 1 ;
%     			while ( x =< 5 ) do {
%     				y = y * x ;
%     				x = x + 1 ;
%     			} ;
% 		    ),		   	
% 	expandEnv([],Env0),
% 	compileHL(Program,Tac,Env0,0,Env1,_),
% 	writeln('==================================='),
% 	writeln('Testing compilation of program:'), 
% 	writeln('Compiled into s|T.A.C|k.:'),
% 	writeTac(Tac),
% 	tacToObj(Tac,Obj),
% 	writeln('Translation into object code:'),
% 	writeObj(Obj,0),
% 	empty_assoc(Empty),
% 	writeln(Env1),
% 	execObj(0,Obj,Empty,Empty,_,HeapOut,[],StackOut),
% 	writeln('Resulting StackOut: '),
% 	writeln(StackOut),
% 	writeln('Resulting HeapOut: '),
% 	writeln(HeapOut),
% 	% writeln('Interpretation of program:'),
% 	% execHL(Program,Env0,EnvInterp,firstTime),
% 	% write('x='), getEnv(x,EnvInterp,Valx), writeln(Valx),
% 	% write('y='), getEnv(y,EnvInterp,Valy), writeln(Valy),
%     write('Address of x:'), getEnv(x,Env1,Addrx), write(Addrx),
% 	write(', value = '), get_assoc(Addrx,HeapOut,Valx), writeln(Valx),
%  	write('Address of y:'), getEnv(y,Env1,Addry), write(Addry),
% 	write(', value = '), get_assoc(Addry,HeapOut,Valy), writeln(Valy).


% :- Program = (
% 				% GCD
% 				int a, b, t;
%     			a = 1071 ;
%     			b = 462 ;

%   				while (b \= 0) do {
%        				t = b ;
%       		 		b = a mod b ;
%        				a = t ;
%        			} ;
% 		    ),		   	
% 	expandEnv([],Env0),
% 	compileHL(Program,Tac,Env0,0,Env1,_),
% 	writeln('==================================='),
% 	writeln('Testing compilation of program:'), 
% 	writeln('Compiled into s|T.A.C|k.:'),
% 	writeTac(Tac),
% 	tacToObj(Tac,Obj),
% 	writeln('Translation into object code:'),
% 	writeObj(Obj,0),
% 	empty_assoc(Empty),
% 	writeln(Env1),
% 	execObj(0,Obj,Empty,Empty,_,HeapOut,[],StackOut),
% 	writeln('Resulting StackOut: '),
% 	writeln(StackOut),
% 	writeln('Resulting HeapOut: '),
% 	writeln(HeapOut),
% 	% writeln('Interpretation of program:'),
% 	% execHL(Program,Env0,EnvInterp,firstTime),
% 	% write('x='), getEnv(x,EnvInterp,Valx), writeln(Valx),
% 	% write('y='), getEnv(y,EnvInterp,Valy), writeln(Valy),
%     write('Address of a:'), getEnv(a,Env1,Addrx), write(Addrx),
% 	write(', value = '), get_assoc(Addrx,HeapOut,Valx), writeln(Valx),
%  	write('Address of b:'), getEnv(b,Env1,Addry), write(Addry),
% 	write(', value = '), get_assoc(Addry,HeapOut,Valy), writeln(Valy).


  :- Program = (
				% Exponentiation 
				int a, x, ans;
				ans = 1 ;
    			a = 2 ;
    			x = 8 ;
    			if (x == 0) then {
    				ans = 1 ;
    			} else {
    				while ( x > 0 )	do {
    					ans = a * ans ;
    					x = x - 1 ;
    				} ;
       			} ;
		    ),		   	
	expandEnv([],Env0),
	compileHL(Program,Tac,Env0,0,Env1,_),
	writeln('==================================='),
	writeln('Testing compilation of program:'), 
	writeln('Compiled into s|T.A.C|k.:'),
	writeTac(Tac),
	tacToObj(Tac,Obj),
	writeln('Translation into object code:'),
	writeObj(Obj,0),
	empty_assoc(Empty),
	writeln(Env1),
	execObj(0,Obj,Empty,Empty,_,HeapOut,[],StackOut),
	writeln('Resulting StackOut: '),
	writeln(StackOut),
	writeln('Resulting HeapOut: '),
	writeln(HeapOut),
    write('Address of ans:'), getEnv(ans,Env1,Addrx), write(Addrx),
	write(', value = '), get_assoc(Addrx,HeapOut,Valx), writeln(Valx).
