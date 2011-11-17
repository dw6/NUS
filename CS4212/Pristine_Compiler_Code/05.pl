/*
 * Compilation of HL language with basic programming constructs:
 * assignment, if, while, arithmetic/boolean expressions
 * New element: scopes, implemented via the type decl 'int x;'
 *  (such declarations may appear anywhere in the code, as in Java,
 *   however only int declarations are allowed)
 *
 * Loosely based on 03.pl (this code does not handle switch statements)
 *
 * TAC language needs to be enhanced to accept memory references
 *    -- memory (heap) is modelled as an association list
 *       with mappings (Address |-> Value).
 *    -- addresses are multiples of 4, as in regular processor
 *       architectures -- useful later when we model strings
 *
 * As usual, we have HL and TAC interpreters to facilitate the
 * test of correctness for the compiler.
 */

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


/*
 * Expression evaluator: implementation of operational semantics of
 * language. This time we do not provide a syntax checker, but we make
 * sure that the evaluator accepts only legal expressions. The call
 * pattern is
 *
 *   evalExpr(Expression,Environment,Value)
 *
 * where Expression and Environment are expected to be bound, and Value
 * is expected to be unbound.
 *
 * This is a more compact version, as compared to 02.pl
 */

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
/*
:- Code = (
	  int i,j ;
	  i = 10 ;
	  while ( i > 0 ) do {
	     int x ;
	     x = i ;
	     x = x - 1 ;
	     i = x ;
	     if ( x < 5 ) then {
		j = ((x < 4) ? 20 : 10) ;
	     };
	  };
	  ),
	expandEnv([],Env0),
	execHL(Code,Env0,Env1,firstTime),
	writeln('==========================='),
	writeln('Interpreting program:'),
	writeln(Code),
	writeln('Results:'),
	getEnv(i,Env1,Vali),
	write('i='),writeln(Vali),
	getEnv(j,Env1,Valj),
	write('j='),writeln(Valj).

:- Code = (
	  int i,j ;
	  i = 10 ;
	  j = 20 ;
	  {
	   int x ;
	   x = i ;
	   i = j ;
	   j = x ;
	  };
	  ),
	expandEnv([],Env0),
	execHL(Code,Env0,Env1,firstTime),
	writeln('==========================='),
	writeln('Interpreting program:'),
	writeln(Code),
	writeln('Results:'),
	getEnv(i,Env1,Vali),
	write('i='),writeln(Vali),
	getEnv(j,Env1,Valj),
	write('j='),writeln(Valj).
*/
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
taiNLtoObj(nop,P,P,nop) :- !.
taiNLtoObj(X=Y,P,[L:K|P],X=Z) :-
	Y =.. [F,L,A], atom(L), !, Z =.. [F,K,A].
taiNLtoObj(X=Y,P,[Y:Z|P],X=Z) :- atom(Y), !.
taiNLtoObj(X=Y,P,P,X=Y) :- !.
taiNLtoObj(Iin,P,[X:Y|P],Iout) :-
        (   Iin = (goto E), Iout = (goto D)
        ;   Iin = (if Expr goto E), Iout = (if Expr goto D) ),
        (   E =.. [F,X,Z], D =.. [F,Y,Z]
        ;   atom(E), E = X, D = Y ) ,!.
%taiNLtoObj(_I,_Pin,_Pout,_T).

% Three address instruction (possibly labelled) --> Object instruction
%    -- add pair (Label,IP) to symbol table
taiToObj(L::nop,IP,Lin,Lout,Pin,Pout,T) :- taiToObj(L::,IP,Lin,Lout,Pin,Pout,T).
taiToObj(L::I,IP,Lin,Lout,Pin,Pout,T) :- !,
        get_assoc(L,Lin,_)
        ->  writeln('Duplicate labels'), abort
        ;   put_assoc(L,Lin,IP,Laux),
            (   I =.. [(::)|_]
             -> taiToObj(I,IP,Laux,Lout,Pin,Pout,T)
             ;  Lout=Laux, taiNLtoObj(I,Pin,Pout,T) ).
taiToObj((L::),IP,Lin,Lout,P,P,none) :-
        get_assoc(L,Lin,_)
        -> writeln('Duplicate labels'), abort
        ;  put_assoc(L,Lin,IP,Lout).
taiToObj(I,_,L,L,Pin,Pout,T) :- taiNLtoObj(I,Pin,Pout,T).

% First pass: translate instructions, accumulate labels and
% corresponding IPs in symbol table, and create placeholders
% for addresses in object code
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

% Second pass: replace the placeholders in code by addresses that
% were computed for labels.
secondPass([],_).
secondPass([Lbl:P|T],L) :-
        (   get_assoc(Lbl,L,P)
	->  true
        ;   P = Lbl ),
	secondPass(T,L).

% Main predicate, applies the two passes to a TAC
tacToObj(SourceCode,ObjectCode) :-
        empty_assoc(Empty),
        firstPass(SourceCode,0,_,Empty,Labels,[],Placeholders,Empty,ObjectCode),
        secondPass(Placeholders,Labels).

% Pretty-printing of TAC
alignLabel(X,Y) :-
        atomic_concat(X,'          ',Z),
        atom_chars(Z,L), append(L1,_,L), length(L1,10),
        atomic_list_concat(L1,Y).

writeTac((X;Y)) :- !, writeTac(X), writeTac(Y).
writeTac((X::Y::Z)) :- !, writeTac((X::);(Y::Z)).
writeTac((X::Y)) :- !, alignLabel(X,X1),writeln((X1::Y)).
writeTac((X::)) :- !, alignLabel(X,X1), write(X1), writeln((::)).
writeTac(X) :- write('            '),writeln(X).

% pretty-printing of Obj
alignIP(X,Y) :-
        atomic_concat('   ',X,Z),
        atom_chars(Z,L), append(_,L1,L), length(L1,3),
        atomic_list_concat(L1,Y).
writeObj(Obj,IP) :- max_assoc(Obj,K,_), IP > K, !.
writeObj(Obj,IP) :-
        get_assoc(IP,Obj,I), alignIP(IP,X),write(X), write(' :: '), writeln(I),
        IPnext is IP + 1, writeObj(Obj,IPnext).

% Operational semantics of object code -- enhanced with references
%    -- New instructions : [Addr] = Expr; Reg = [Addr],
evalTaiRhs(X,Env,Val) :- atom(X), !, get_assoc(X,Env,Val).
evalTaiRhs(X,_,X) :- integer(X), !.
evalTaiRhs(E,Env,Val) :-
	E =.. [F,A,B], member(F,[+,-,*,/,mod,<<,>>,and,or,xor,/\,\/]),
	(atom(A),!;integer(A) ), (atom(B),!;integer(B)),
	evalTaiRhs(A,Env,ValA), evalTaiRhs(B,Env,ValB),
	(   F == / -> C = ValA // ValB ; C =.. [F,ValA,ValB]), Val is C.

getHeap(E,Env,Heap,Addr,Val) :-
	evalTaiRhs(E,Env,Addr),
	(   get_assoc(Addr,Heap,Val)
	->  true
	;   Val = undef
	).

execTai(nop,Env,Heap,Env,Heap) :- !.
execTai((LHS=RHS),Env,Heap,NewEnv,NewHeap) :-
	(   atom(LHS)
	->  (   RHS = [AddrExpr]
	    ->  getHeap(AddrExpr,Env,Heap,_Addr,Val),
		put_assoc(LHS,Env,Val,NewEnv), Heap=NewHeap
	    ;	evalTaiRhs(RHS,Env,Val), put_assoc(LHS,Env,Val,NewEnv),
		Heap=NewHeap )
	;   LHS = [LhsAddr], getHeap(LhsAddr,Env,Heap,Addr,_Val), NewEnv = Env,
	    (	atom(RHS)
	    ->	get_assoc(RHS,Env,Val)
	    ;	(   integer(RHS)
		->  Val = RHS
		;   write('Incorrect instruction : '), writeln(LHS=RHS), abort )),
	    put_assoc(Addr,Heap,Val,NewHeap)).

% :- dynamic counter/1.
% :- retractall(counter(_)).

% counter(0).
execObj(IP,Code,Env,Heap,Env,Heap) :- max_assoc(Code,K,_), IP > K, !.
execObj(IP,Code,EnvIn,HeapIn,EnvOut,HeapOut) :-
	% retract(counter(Cnt)),Cnt1 is Cnt+1,assert(counter(Cnt1)),
	% (   Cnt1 < 10000 -> true ; writeln(HeapIn), abort ),
        get_assoc(IP,Code,Instr),
	% writeln(HeapIn), writeln(EnvIn),
	% write(IP),write(::),writeln(Instr),%read(_),
        (   Instr = (goto L), !, evalExpr(L,[EnvIn],IPnext), EnvAux = EnvIn, HeapAux = HeapIn
        ;   Instr = (if E goto L), !, evalExpr(E,[EnvIn],Val), EnvAux = EnvIn, HeapAux = HeapIn,
                  ( Val = 1
                    -> evalExpr(L,[EnvIn],IPnext)
                    ;  IPnext is IP + 1 )
        ;   execTai(Instr,EnvIn,HeapIn,EnvAux,HeapAux), IPnext is IP+1 ),
        execObj(IPnext,Code,EnvAux,HeapAux,EnvOut,HeapOut).


% Test of TAC and Obj

:- Code = ([0] = 1 ;
	   [4] = 2 ;
	   r_1 = [0] ;
	   [8] = 4 ;
	   r_2 = 4 ;
	   [0] = r_2 ;
lbl:: ;
lbl1::
current::  if r_2 > 0 goto current+r_2 ;
	   nop ;
	   goto current ;
	   goto current - r_2 ;
	   r_3 = 100 ;
	   goto lbl+7 ;
	   goto lbl1 ;
	  ),
	  writeln('=========================='),
	  writeln('TAC code to be translated and executed:'),
	  writeTac(Code), tacToObj(Code,Obj),
	  writeln('Translation into object code:'),
	  writeObj(Obj,0),
	  empty_assoc(Empty),
	  execObj(0,Obj,Empty,Empty,EnvOut,HeapOut),
	  writeln('Result of execution:'),
	  get_assoc(r_1,EnvOut,Val1),
	  write('r_1 = '),writeln(Val1),
	  get_assoc(r_2,EnvOut,Val2),
	  write('r_2 = '),writeln(Val2),
	  get_assoc(0,HeapOut,Heap0),
	  write('[0] = '),writeln(Heap0),
	  get_assoc(4,HeapOut,Heap4),
	  write('[4] = '),writeln(Heap4),
	  get_assoc(8,HeapOut,Heap8),
	  write('[8] = '),writeln(Heap8).


/********************************************
 * Compiler from HL language to TAC
 ********************************************/

:- dynamic auxreg/1.
:- dynamic auxvar/1.

% generate new register names - same as 02.pl, but renamed predicate
%  ( the variables of 02.pl are now called registers; variables are
%    stored in memory, and are allocated addresses )
newreg(X) :- atom(X),!.
newreg(X) :-
        retract(auxreg(Y))
        ->  Y1 is Y+1, assert(auxreg(Y1)), atomic_concat('r_',Y1,X)
        ;   assert(auxreg(0)), X = 'r_0' .

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

% compilation of expressions, similar to 02.pl
%   -- each program variable is encoded as [Addr], for some
%      automatically allocated address
%   -- second argument: code resulting from compilation
%   -- third: place where result is stored
%   -- environment contains mappings of variables to addresses
%   -- the last two arguments are label requests: in and out

compileExpr(X,R=[Addr],R,Env) :-
	atom(X), !, newreg(R), getEnv(X,Env,Addr).
compileExpr(X,nop,X,_Env) :-
	integer(X), !.
compileExpr(E,Code,Result,Env) :-
	E =.. [F,A,B], member(F,[+,-,*,/,mod,<<,>>,/\,\/,and,or,xor]), !,
	compileExpr(A,CodeA,RA,Env),
	compileExpr(B,CodeB,RB,Env),
	Op =.. [F,RA,RB], newreg(Result),
	C = (Result = Op),
	Code = (CodeA;CodeB;C).
compileExpr(E,Code,Result,Env) :-
	E =.. [F,A,B], member(F,[<,>,=<,>=,==,\=]), !,
	compileExpr(A-B,CodeAB,R,Env),
	Op =.. [F,R,0], newreg(Result), newlabel(LblOut), newlabel(Skip),
	C = ( if Op goto Skip ; Result = 0 ; goto LblOut ; Skip::Result = 1; LblOut:: ),
	Code = (CodeAB;C).
compileExpr((X ? Y : Z),Code,R,Env) :- !,
        newreg(R), newlabel(Skip), newlabel(Lout),
        compileExpr(X,Cx,Qx,Env),
        compileExpr(Y,Cy,Qy,Env),
        compileExpr(Z,Cz,Qz,Env),
	C1 = (if Qx == 0 goto Skip),
        C2 = (R = Qy ; goto Lout),
        C3 =  (R = Qz),
        Code = (Cx ; C1 ; Cy ; C2 ; Skip::; Cz; C3; Lout::).
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

compileStmt((int L),nop,EnvIn,TopIn,EnvOut,TopOut) :-
	newvars(L,EnvIn,TopIn,EnvOut,TopOut).
compileStmt((X=E),(CE;C),Env,Top,Env,Top) :-
	compileExpr(E,CE,R,Env),
	(   getEnv(X,Env,Addr)
	->  true
	;   write('Undeclared variable: '),
	    writeln(X), abort ),
	C = ( [Addr] = R ).
compileStmt((if B then { S1 } else { S2 }),Code,Env,Top,Env,Top) :-
	compileExpr(B,CB,RB,Env),
	C1 = (if RB == 0 goto LblS2),
	newlabel(LblS2),
	compileStmt({S1},CS1,Env,Top,_,_),
	C2 = (goto LblOut),
	compileStmt({S2},CS2,Env,Top,_,_),
	newlabel(LblOut),
	Code = (CB;C1;CS1;C2;LblS2::;CS2;LblOut::).
compileStmt((if B then { S }),Code,Env,Top,Env,Top) :-
	compileExpr(B,CB,RB,Env),
	C = (if RB == 0 goto LblOut),
	newlabel(LblOut),
	compileStmt({S},CS,Env,Top,_,_),
	Code = (CB;C;CS;LblOut::).
compileStmt((while B do { S }),Code,Env,Top,Env,Top) :-
	newlabel(LblTop), newlabel(LblOut),
	compileExpr(B,CB,RB,Env),
	C1 = (if RB == 0 goto LblOut),
	compileStmt({S},CS,Env,Top,_,_),
	C2 = ( goto LblTop ),
	Code = (LblTop::;CB;C1;CS;C2;LblOut::).
compileStmt({S},Code,Env,Top,Env,Top) :-
	expandEnv(Env,NewEnv),
	compileHL(S,Code,NewEnv,Top,_,_).

% Top level compilation predicate. It accepts only programs
% with correct syntax.

compileHL((S1;S2),(C1;C2),Env,Top,NewEnv,NewTop) :-
	compileStmt(S1,C1,Env,Top,AuxEnv,AuxTop),
	compileHL(S2,C2,AuxEnv,AuxTop,NewEnv,NewTop).
compileHL((S;),Code,Env,Top,NewEnv,NewTop) :-
	compileStmt(S,Code,Env,Top,NewEnv,NewTop).

% Compiler test

:- resetnewreg, resetnewlabel.


:- Program = (
	     int x, y ;
	     x = -1 ;
	     if (x < 0) then
	      {
		int x; x = 2 ; y = x ;
		while ( y > 0 ) do
		 {
		  int z ;
		  z = x*x ;
		  y = y / 2 ;
		  if ( z >= y ) then
		   {
		    z = z - 1 ;
		   }
		  else
		   {
		    z = z + 1 ;
		   } ;
		 } ;
		y = ( (y>0) ? (x+1) : (x-1) ) ;
	      } ;
	     x=1;
	     ),
	expandEnv([],Env0),
	compileHL(Program,Tac,Env0,0,Env1,_),
	writeln('==================================='),
	writeln('Testing compilation of program:'), writeln(Program),
	writeln('Compiled into TAC:'),
	writeTac(Tac),
	tacToObj(Tac,Obj),
	writeln('Translation into object code:'),
	writeObj(Obj,0),
	empty_assoc(Empty),
	writeln('Interpretation of program:'),
	execHL(Program,Env0,EnvInterp,firstTime),
	write('x='), getEnv(x,EnvInterp,Valx), writeln(Valx),
	write('y='), getEnv(y,EnvInterp,Valy), writeln(Valy),
	execObj(0,Obj,Empty,Empty,_,HeapOut),
        write('Address of x:'), getEnv(x,Env1,Addrx), write(Addrx),
	write(', value = '), get_assoc(Addrx,HeapOut,Valx), writeln(Valx),
        write('Address of y:'), getEnv(y,Env1,Addry), write(Addry),
	write(', value = '), get_assoc(Addry,HeapOut,Valy), writeln(Valy).

% Test compiler

:- resetnewreg, resetnewlabel.

:- Program = (
	  int x, y;
          x = 144 ;
          y = 60 ;
          while ( x \= y ) do {
             if ( x < y ) then {
		int z ;
                z = y - x ;
		y = z ;
             } else {
		int w ;
                w = x - y ;
		x = w ;
             } ;
          } ;
	     ),
	expandEnv([],Env0),
	compileHL(Program,Tac,Env0,0,Env1,_),
	writeln('==================================='),
	writeln('Testing compilation of program:'), writeln(Program),
	writeln('Compiled into TAC:'),
	writeTac(Tac),
	tacToObj(Tac,Obj),
	writeln('Translation into object code:'),
	writeObj(Obj,0),
	empty_assoc(Empty),
	writeln('Interpretation of program:'),
	execHL(Program,Env0,EnvInterp,firstTime),
	write('x='), getEnv(x,EnvInterp,Valx), writeln(Valx),
	write('y='), getEnv(y,EnvInterp,Valy), writeln(Valy),
	execObj(0,Obj,Empty,Empty,_,HeapOut),
        write('Address of x:'), getEnv(x,Env1,Addrx), write(Addrx),
	write(', value = '), get_assoc(Addrx,HeapOut,Valx), writeln(Valx),
        write('Address of y:'), getEnv(y,Env1,Addry), write(Addry),
	write(', value = '), get_assoc(Addry,HeapOut,Valy), writeln(Valy).



















