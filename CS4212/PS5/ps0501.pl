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
:- op(200,fx,^^).
:- op(100,xfx,#).
:- op(950,fx,return).

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

varInEnv(X,[Top|Rest]) :-
	get_assoc(X,Top,_),! ; varInEnv(X,Rest).

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
%   -- makes it possible to track uninitialized variables

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

globalEnv([E],[E]) :- !.
globalEnv([_,E|T],X) :- globalEnv([E|T],X).

replaceGlobalEnv(Genv,[_],Genv) :- !.
replaceGlobalEnv(Genv,[E|T],[E|X]) :- replaceGlobalEnv(Genv,T,X).

combineEnvWhile([E|_],Env,[E|Env]).

% API to handle procedure definitions

isProcedure(int _#_::_):- !.
isProcedure(_#_::_).

procedureName(int F#_::_,F) :- !.
procedureName(F#_::_,F).

procedureType(int _#_::_,int) :- !.
procedureType(_#_::_,void).

procedureArgs(int _#Args::_,Args) :- !.
procedureArgs(_#Args::_,Args).

procedureBody(int _#_::Body,Body) :- !.
procedureBody(_#_::Body,Body).

% handle bindings between formal and actual arguments
argBindingsHelper([],[],Env,Env):-!.
argBindingsHelper([A|RA],[PA|RPA],EnvIn,EnvOut) :-
	createEnv(PA,EnvIn,EnvAux1),
	putEnv(PA,EnvAux1,A,EnvAux2),
	argBindingsHelper(RA,RPA,EnvAux2,EnvOut).

argumentBindings(Args,Pdef,EnvIn,EnvArgs) :-
	globalEnv(EnvIn,GEnv),
	expandEnv(GEnv,EnvAux),
	procedureArgs(Pdef,Pargs),
	argBindingsHelper(Args,Pargs,EnvAux,EnvAux2),
	createEnv(procedureReturnVar,EnvAux2,EnvArgs).

% clean up the environment after procedure call in interpreter
cleanUp(EnvOrig,EnvProcOut,EnvOut) :-
	globalEnv(EnvProcOut,GEnv),
	replaceGlobalEnv(GEnv,EnvOrig,EnvOut).

/*
 *  HL expression evaluator: allows procedure calls
 *   -- since procedure calls may change values of global variables
 *      we need input and output environments here too
 *   -- environments are now stacks of records; global variables are
 *      at the bottom of the stack
 *   -- evaluator also takes in a dictionary of procedure definitions
 *      to be able to execute a procedure call when one is encountered
 */

evalExpr(X,Env,Env,_Procs,Val) :-
	atom(X),!, \+ atom_prefix(X,v_), getEnv(X,Env,Val),
	(   Val = undef
	->  write('Variable '), write(X), writeln(' unitialized.'), abort
	;   true ).
evalExpr(X,Env,Env,_Procs,X) :- integer(X),!.
evalExpr(E,EnvIn,EnvOut,Procs,Val) :-
	E =.. [F,A,B], member(F,[+,-,*,/,mod,and,or,/\,\/,<<,>>,xor]), !,
	evalExpr(A,EnvIn,EnvAux,Procs,ValA),
	evalExpr(B,EnvAux,EnvOut,Procs,ValB),
        ( F == / -> C = ValA // ValB ; 	C =.. [F,ValA,ValB]), Val is C.
evalExpr(E,EnvIn,EnvOut,Procs,Val) :-
	E =.. [F,A,B], member(F,[<,>,=<,>=,==,\= ]), !,
	evalExpr(A,EnvIn,EnvAux,Procs,ValA),
	evalExpr(B,EnvAux,EnvOut,Procs,ValB),
	C =.. [F,ValA,ValB],
	(   C -> Val = 1 ; Val = 0 ).


evalExpr(E,EnvIn,EnvOut,Procs,Val) :-
	writeln('^^ Operator!'),
	E =.. [F,A], member(F,[^^]),!,
	evalExpr(A,EnvIn,EnvOut,Procs,ValA),
	C =.. [F,ValA], Val is C.





evalExpr(E,EnvIn,EnvOut,Procs,Val) :-
	E =.. [F,A], member(F,[+,-,\]),!,
	evalExpr(A,EnvIn,EnvOut,Procs,ValA),
	C =.. [F,ValA], Val is C.





evalExpr((X ? Y : Z),EnvIn,EnvOut,Procs,Val) :- !,
	evalExpr(X,EnvIn,EnvAux,Procs,ValA),
	(   ValA \= 0
	->  evalExpr(Y,EnvAux,EnvOut,Procs,Val)
	;   evalExpr(Z,EnvAux,EnvOut,Procs,Val) ).
evalExpr(P#Args,EnvIn,EnvOut,Procs,Val) :-  % environment of procedure body
	get_assoc(P,Procs,Pdef),	    % is global vars + argument bindings
	procedureType(Pdef,Type),
	(   Type == void, Val \= noval
	->  write('Illegal procedure call:'), writeln(P), abort
	;   true ),
	evalArgs(Args,ArgVals,Pdef,Procs,EnvIn,EnvAux),
	argumentBindings(ArgVals,Pdef,EnvAux,EnvArgs),
	procedureBody(Pdef,Pbody),
	execStmt(Pbody,Procs,EnvArgs,EnvArgsOut,firstTime,_ReturnEncountered),
	getEnv(procedureReturnVar,EnvArgsOut,RetVal),
	(   Val == noval
	->  (   RetVal == noval
	    ->	true
	    ;	write('Invalid return value from procedure:'), writeln(P), abort )
	;   (   RetVal == noval
	    ->	write('Invalid return value from procedure:'), writeln(P), abort
	    ;	Val = RetVal ) ),
	cleanUp(EnvIn,EnvArgsOut,EnvOut).

% Predicate to convert a list of expressions into the corresponding
% list of values; useful in computing the list of actual arguments
% in a procedure call.
evalArgs([],[],_Pdef,_Procs,Env,Env) :- !.
evalArgs([Arg|Args],[ArgVal|ArgVals],Pdef,Procs,EnvIn,EnvOut) :-
	evalExpr(Arg,EnvIn,EnvAux,Procs,ArgVal),
	evalArgs(Args,ArgVals,Pdef,Procs,EnvAux,EnvOut).

% helper predicate to handle a list of variables in a declaration
% each variable in the list is added to the current environment
createVars( (X,Y), EnvIn, EnvOut ) :- !,
	createVars(X,EnvIn,EnvAux), createVars(Y,EnvAux,EnvOut).
createVars( X, EnvIn, EnvOut ) :-
	(   atom(X), \+ atom_prefix(X,v_)
	->  createEnv(X,EnvIn,EnvOut)
	;   write('Incorrect variable name:'), writeln(X), abort ).

% Statement execution predicate, has a new element: variable
% declarations
%    -- the fifth argument is useful in handling 'while' loops
%    -- when entering 'while' loop body a second time, all declarations
%       must be disregarded
%    -- Fifth argument possible values:
%	  'firstTime' : used when a stmt is executed for the first time
%	  'secondTime': used when a stmt is subsequently executed again
%    -- Sixth argument: signals that a return has been encountered;
%	execution of statements must be disabled till the end of
%	procedure is encountered

execStmt((int L),_Procs,EnvIn,EnvOut,Flag,_) :- !,
	(   Flag == firstTime
	->  createVars(L,EnvIn,EnvOut)
	;   EnvOut = EnvIn ).
execStmt((X=E),Procs,EnvIn,EnvOut,_,_) :- !,
	evalExpr(E,EnvIn,EnvAux,Procs,Val), putEnv(X,EnvAux,Val,EnvOut).
execStmt((while B do { S }),Procs,EnvIn,EnvOut,firstTime,Ret) :-!,
	 evalExpr(B,EnvIn,EnvAux,Procs,Val),
	 (   Val \= 0
	 ->  expandEnv(EnvAux,Env1),
	     execHL(S,Procs,Env1,Env2,firstTime,RetS),
	     (	 RetS == returnEncountered
	     ->	 Ret = RetS, Env3 = Env2
	     ;	 execStmt((while B do { S }),Procs,Env2,Env3,secondTime,Ret) ),
	     shrinkEnv(Env3,EnvOut)
	 ;   EnvOut = EnvAux ).
execStmt((while B do { S }),Procs,EnvIn,EnvOut,secondTime,Ret) :-!,
	 shrinkEnv(EnvIn,EnvB),
	 evalExpr(B,EnvB,EnvAux,Procs,Val),
	 combineEnvWhile(EnvIn,EnvAux,EnvBody),
	 (   Val \= 0
	 ->  execHL(S,Procs,EnvBody,Env2,secondTime,RetS),
	     (	 RetS == returnEncountered
	     ->	 Ret = RetS, EnvOut = Env2
	     ;	 execStmt((while B do { S }),Procs,Env2,EnvOut,secondTime,Ret) )
	 ;   EnvOut = EnvBody).
execStmt((if B then { S1 } else { S2 }),Procs,EnvIn,EnvOut,_,Ret) :- !,
        evalExpr(B,EnvIn,EnvAux,Procs,Val),
	expandEnv(EnvAux,Env1),
        (   Val \= 0
        ->  execHL(S1,Procs,Env1,Env2,firstTime,Ret)
        ;   execHL(S2,Procs,Env1,Env2,firstTime,Ret) ),
	shrinkEnv(Env2,EnvOut).
execStmt((if B then { S } ),Procs,EnvIn,EnvOut,_,Ret) :- !,
        evalExpr(B,EnvIn,EnvAux,Procs,Val),
	expandEnv(EnvAux,Env1),
        (   Val \= 0
        ->  execHL(S,Procs,Env1,Env2,firstTime,Ret)
        ;   Env1 = Env2 ),
	shrinkEnv(Env2,EnvOut).
execStmt({S},Procs,EnvIn,EnvOut,_,Ret) :-
	expandEnv(EnvIn,Env1),
	execHL(S,Procs,Env1,Env2,firstTime,Ret),
	shrinkEnv(Env2,EnvOut).
execStmt(P#Args,Procs,EnvIn,EnvOut,_,_) :- !,
	evalExpr(P#Args,EnvIn,EnvOut,Procs,noval).
execStmt((return X),Procs,EnvIn,EnvOut,_,returnEncountered) :- !,
	evalExpr(X,EnvIn,EnvAux,Procs,Val),
        putEnv(procedureReturnVar,EnvAux,Val,EnvOut).
execStmt(return,_Procs,EnvIn,EnvOut,_,returnEncountered) :- !,
	putEnv(procedureReturnVar,EnvIn,noval,EnvOut).

execHL((S1;S2),Procs,EnvIn,EnvOut,Flag,Ret) :- !,
	execStmt(S1,Procs,EnvIn,EnvAux,Flag,Ret1),
	(   Ret1 == returnEncountered
	->  Ret = Ret1, EnvOut = EnvAux
	;   execHL(S2,Procs,EnvAux,EnvOut,Flag,Ret) ).
execHL((S;),Procs,EnvIn,EnvOut,Flag,Ret) :- !,
	execStmt(S,Procs,EnvIn,EnvOut,Flag,Ret).

% Main predicate: execute HL with Procedures
execHLP( (P;Rest),Pin,Pout,EnvOut) :-
	isProcedure(P), !,
	procedureName(P,Pname),
	(   get_assoc(Pname,Pin,_)
	->  write('Duplicate procedure name:'), writeln(Pname), abort
	;   true ),
	put_assoc(Pname,Pin,P,Paux), % collect procedure definitions
	execHLP(Rest,Paux,Pout,EnvOut).
execHLP(P,Pin,_,EnvOut) :-
	expandEnv([],Empty), % execute main program starting with empty env
	execHL(P,Pin,Empty,EnvOut,firstTime,_ReturnEncountered).

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
        get_assoc(IP,Code,Instr), empty_assoc(Empty),
	% writeln(HeapIn), writeln(EnvIn),
	% write(IP),write(::),writeln(Instr),%read(_),
        (   Instr = (goto L), !, evalExpr(L,[EnvIn],_,Empty,IPnext), EnvAux = EnvIn, HeapAux = HeapIn
        ;   Instr = (if E goto L), !, evalExpr(E,[EnvIn],_,Empty,Val), EnvAux = EnvIn, HeapAux = HeapIn,
                  ( Val = 1
                    -> evalExpr(L,[EnvIn],_,Empty,IPnext)
                    ;  IPnext is IP + 1 )
        ;   execTai(Instr,EnvIn,HeapIn,EnvAux,HeapAux), IPnext is IP+1 ),
        execObj(IPnext,Code,EnvAux,HeapAux,EnvOut,HeapOut).


/********************************************
 * Compiler from HL language to TAC
 ********************************************/
% - Local variables (inner scope variables) are now placed on the stack
% - Use 'stacPointer' and 'framePointer' as register that implement the
%   stack and activation records
% - global variables still in the "heap", having lower addresses
% - activation records have textbook implementation
%    -- caller places arguments on the stack
%    -- callee creates room for local procedure variables
%    -- procedure returns value in register 'returnValue'
%    -- callee cleans up local variables
%    -- caller cleans up arguments
%


:- dynamic auxreg/1.
:- dynamic auxvar/1.

% generate new register names - same as 02.pl, but renamed predicate
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
	createEnv(X,EnvIn,EnvAux), putEnv(X,EnvAux,TopOut,EnvOut).


newvars((V,Vs),EnvIn,TopIn,EnvOut,TopOut) :-
	newvars(V,EnvIn,TopIn,EnvAux,TopAux),
	newvars(Vs,EnvAux,TopAux,EnvOut,TopOut).

% compilation of expressions, similar to 05.pl
%   - environment is split between locals and globals
%   - procedures are lexically scoped, they only inherit the global
%     variables from their context
%   - identifiers are searched in local environment first, and then in
%     the global environment

compileExpr(X,Code,Reg,_Procs,Globs,Locs) :-

	atom(X), !, newreg(Reg),
	(   varInEnv(X,Locs) 	
	->  getEnv(X,Locs,Offset),
	    (   Offset > 0
	    ->  C = ( Reg = [framePtr - Offset] )
	    ;   O is - Offset,C = (Reg = [framePtr+O]) )
	;   (   getEnv(X,Globs,Addr)
	    ->	C = ( Reg = [Addr] )
	    ;	write('Undeclared variable: '), writeln(X), abort)
	), Code = C, true,
	writeln(X),
	writeln(Code).


compileExpr(X,nop,X,_Procs,_Globs,_Locs) :- integer(X), !.


compileExpr(E,Code,Result,Procs,Globs,Locs) :-
	E =.. [F,A,B], member(F,[+,-,*,/,mod,<<,>>,/\,\/,and,or,xor]), !,
	compileExpr(A,CodeA,RA,Procs,Globs,Locs),
	compileExpr(B,CodeB,RB,Procs,Globs,Locs),
	Op =.. [F,RA,RB], newreg(Result),
	C = (Result = Op),
	Code = (CodeA;CodeB;C).


compileExpr(E,Code,Result,Procs,Globs,Locs) :-
	E =.. [F,A,B], member(F,[<,>,=<,>=,==,\=]), !,
	(   B == 0
	->  compileExpr(A,CodeAB,R,Procs,Globs,Locs)
	;   compileExpr(A-B,CodeAB,R,Procs,Globs,Locs) ),
	Op =.. [F,R,0], newreg(Result), newlabel(LblOut), newlabel(Skip),
	C = ( if Op goto Skip ;
              Result = 0 ;
              goto LblOut ;
              Skip::Result = 1;
	      LblOut :: ),
	Code = (CodeAB;C).

% Handle the code here!
% GlobHead | GlobTail 
% LocHead | LocTail 


% This would lead to an error condition
compileExpr(E,_Code,_R,_Procs,[],_) :-
	E =.. [^^,_], !,
	writeln('Error in ^^'), abort.


compileExpr(E,Code,R,Procs,[_|GT],[]) :-
	E =.. [^^,A], !,
	newreg(R),
	compileExpr(A,Code,R,Procs,GT,[]), !,
	Code = (CodeA ; R = R1).

compileExpr(E,Code,R,Procs,Globs,[_|LT]) :-
	E =.. [^^,A], !,
	newreg(R),
	compileExpr(A,CodeA,R1,Procs,Globs,LT), 
	Code = (CodeA ; R = R1).


compileExpr(E,Code,R,Procs,Globs,Locs) :-
	E =.. [F,A], member(F,[+,-]), !,
	C =.. [F,0,A], 
	compileExpr(C,Code,R,Procs,Globs,Locs), !.




compileExpr((X ? Y : Z),Code,R,Procs,Globs,Locs) :- !,
        newreg(R), newlabel(Skip), newlabel(Lout),
        compileExpr(X,Cx,Qx,Procs,Globs,Locs),
        compileExpr(Y,Cy,Qy,Procs,Globs,Locs),
        compileExpr(Z,Cz,Qz,Procs,Globs,Locs),
        C1 = (if Qx == 0 goto Skip),
        C2 = (R = Qy ; goto Lout),
        C3 =  (R = Qz),
        Code = (Cx;C1;Cy;C2;Skip::;Cz;C3;Lout::).


compileExpr(\ X,Code,R,Procs,Globs,Locs) :- !,
	compileExpr((-1) xor X, Code, R, Procs, Globs, Locs).


compileExpr(P#Args,Code,R,Procs,Globs,Locs) :- !,
	get_assoc(P,Procs,Pdef), procedureType(Pdef,Type),
	(   (R == noval, Type == int ; var(R), Type == void)
	    ->	write('Type violation in procedure call: '), writeln(P), abort
	    ;	true ),
	compileArgs(Args,CodeArgs,Procs,Globs,Locs,StackSpace),
	newlabel(Label),
	C = (  Label::stackPtr = stackPtr - 4 ;
		      RetAddr =  Label + 4 ;
		      [stackPtr] = RetAddr ;
	              goto P ;
		      stackPtr = stackPtr + StackSpace
	             ),
	(   R == noval -> true ; R = returnResult ),
	newreg(RetAddr),
	Code = ( CodeArgs ; C ).



% Compilation of statements
% Arg1 (in) : HL statement to be compiled
% Arg2 (out): Compiled code
% Arg3 (in) : Dictionary of procedures, to resolve procedure calls
% Arg4 (in) : input global environment
% Arg5 (in) : input top of heap (first unused addr in global
%             environment)
% Arg6 (out): output global environment (only changed by declarations)
% Arg7 (out): output top of heap (useful for fast allocation of mem
%	      locations)
% Arg8 (in) : input local env : mappings of identifiers into stack
%             addresses
% Arg9 (in) : input top of local env (first unused addr on the current
%             stack)
% Arg10(out): output local env
% Arg11(out): output top of local env
% Arg12(in) : current maximum of stack space used
% Arg13(out): new maximum of stack space used, computed after a
%             declaration
% Arg14(in) : level of scope, 0 : global scope, >0 : local scope
% Arg15(in) : label that denotes the exit from the current procedure

compileStmt( (int L), nop,_Procs,GlobIn,TGIn,GlobOut,TGOut,
	     LocIn,TLIn,LocOut,TLOut,MaxTopIn,MaxTopOut,
	     Lev,_RetLbl) :- !,
	(   Lev == 0
	->  newvars(L,GlobIn,TGIn,GlobOut,TGOut), LocIn = LocOut,
	    TLIn = TLOut, MaxTopIn = MaxTopOut
	;   newvars(L,LocIn,TLIn,LocOut,TLOut), GlobIn = GlobOut,
	    TGIn = TGOut, MaxTopOut is max(MaxTopIn,TLOut) ).


compileStmt( (X=E),Code,Procs,Globs,TG,Globs,TG,
	     Locs,TL,Locs,TL,MaxTop,MaxTop,
	     _Lev,_RetLbl) :- !,
	compileExpr(E,CE,R,Procs,Globs,Locs),
	(   atom(X)  ->  true ;	write('Invalid LHS:'),writeln(X) ),
	(   varInEnv(X,Locs)
	->  getEnv(X,Locs,Offset),
	    (   Offset > 0
	    ->  LHS = [framePtr - Offset]
	    ;   O is - Offset, LHS = [framePtr+O] )
	;   getEnv(X,Globs,Addr), LHS = [Addr]  ),
	C = ( LHS = R ),
	Code = (CE ; C).


compileStmt( (if B then { S1 } else { S2 }), Code,Procs,Globs,TG,Globs,TG,
	     Locs,TL,Locs,TL,MaxTopIn,MaxTopOut,_Lev,RetLbl) :- !,
	compileExpr(B,CB,RB,Procs,Globs,Locs),
	C1 = (if RB == 0 goto LblS2),
	newlabel(LblS2),
	compileStmt({S1},CS1,Procs,Globs,TG,_,_,Locs,TL,_,_,
		    MaxTopIn,MaxTop1,1,RetLbl),
	C2 = (goto LblOut),
	compileStmt({S2},CS2,Procs,Globs,TG,_,_,Locs,TL,_,_,
		    MaxTop1,MaxTopOut,1,RetLbl),
	newlabel(LblOut),
	Code = ( CB ; C1 ; CS1 ; C2 ; LblS2:: ; CS2 ; LblOut:: ).


compileStmt( (if B then { S }), Code,Procs,Globs,TG,Globs,TG,
	     Locs,TL,Locs,TL,MaxTopIn,MaxTopOut,
	     _Lev,RetLbl) :- !,
	compileExpr(B,CB,RB,Procs,Globs,Locs),
	C = (if RB == 0 goto LblOut),
	compileStmt({S},CS,Procs,Globs,TG,_,_,
		    Locs,TL,_,_,MaxTopIn,MaxTopOut,
		    1,RetLbl),
	newlabel(LblOut),
	Code = ( CB ; C ; CS ; LblOut:: ).


compileStmt( (while B do { S }), Code,Procs,Globs,TG,Globs,TG,
	     Locs,TL,Locs,TL,MaxTopIn,MaxTopOut,
	     _Lev,RetLbl) :- !,
	newlabel(LblTop),
	newlabel(LblOut),
	compileExpr(B,CB,RB,Procs,Globs,Locs),
	C1 = (if RB == 0 goto LblOut),
	compileStmt({S},CS,Procs,Globs,TG,_,_,Locs,TL,_,_,
		    MaxTopIn,MaxTopOut,1,RetLbl),
	C2 = ( goto LblTop ),
	Code = ( LblTop:: ; CB ; C1 ; CS ; C2 ; LblOut:: ).


compileStmt( {S}, Code,Procs,Globs,TG,Globs,TG,Locs,TL,Locs,
	     TL,MaxTopIn,MaxTopOut,_Lev,RetLbl) :- !,
	expandEnv(Locs,NewLocs),
	compileHL(S,Code,Procs,Globs,TG,_,_,NewLocs,TL,_,_,
		  MaxTopIn,MaxTopOut,1,RetLbl).


compileStmt( P#Args, Code,Procs,Globs,TG,Globs,TG,
	    Locs,TL,Locs,TL,MaxTop,MaxTop,
	    _Lev,_RetLbl) :- !,
	compileExpr(P#Args,Code,noval,Procs,Globs,Locs).


compileStmt( return, goto RetLbl,_Procs,Glob,TG,Glob,TG,Loc,TL,Loc,TL,
	     MaxTop,MaxTop,_Lev,RetLbl) :- !.


compileStmt( (return X), Code,Procs,Glob,TG,Glob,TG,
	     Loc,TL,Loc,TL,MaxTop,MaxTop,
	     _Lev,RetLbl) :-
	compileExpr(X,CX,R,Procs,Glob,Loc),
	C = ( returnResult = R ; goto RetLbl ),
	Code = (CX ; C ).

% compile list of arguments to a procedure
%   - argument expressions are compiled one by one
%   - each argument is pushed on the stack
compileArgs([],nop,_Procs,_Globs,_Locs,0) :- !.


compileArgs([A|As],Code,Procs,Globs,Locs,StackSize) :-
	compileExpr(A,CA,RA,Procs,Globs,Locs), newreg(Reg),
	C = ( stackPtr = stackPtr - 4 ;
	      Reg = RA ;
	      [stackPtr] = Reg ),
	compileArgs(As,CAs,Procs,Globs,Locs,StackSizeAs),
	StackSize is StackSizeAs+4, Code = ( CA ; C ; CAs).

% allocate offsets on the stack for local procedure variables
argumentsEnv([],Empty,-8) :- !,expandEnv([],Empty).


argumentsEnv([A|As],Env,K) :-
	argumentsEnv(As,Env1,K1),
	createEnv(A,Env1,Env2),
	putEnv(A,Env2,K1,Env),
	K is K1 - 4.

% compilation of procedures
%   - the most important aspect is the environment
%   - new environment
%     -- identifiers map to offsets wrt 'framePointer'
%     -- arguments map to positive offsets
%     -- local variables map into negative offsets
%   - 'stackPointer' must be decreased to make room for the maximum
%     stack usage computed for the body of the procedure
%   - body of procedure compiles as a regular statement in
%     the created environment
compileProcedure(Pdef,Code,Procs,Glob) :-
	procedureName(Pdef,P),
	procedureArgs(Pdef,Pargs),
	procedureBody(Pdef,Pbody),
	argumentsEnv(Pargs,Locs,_),
	newreg(RetAddr),
	compileStmt(Pbody,CodeBody,Procs,Glob,0,_GlobOut,_TGOut,
		    Locs,0,_LocsOut,_TLOut,0,MaxStack,1,RetLbl),
	(   MaxStack == 0
	->  StackAllocation = nop
	;   StackAllocation = ( stackPtr = stackPtr - MaxStack ) ),
	C1 = (  P :: stackPtr = stackPtr - 4 ;
	             [stackPtr] = framePtr ;
                     framePtr = stackPtr
	     ),
	newlabel(RetLbl),
	C2 = (  RetLbl :: stackPtr = framePtr + 8;
                          RetAddr  = [framePtr+4] ;
	                  framePtr = [framePtr] ;
	                  goto RetAddr  ),
	Code = ( C1 ; StackAllocation ; CodeBody ; C2).

% compile all procedures collected while traversing the code
compileProcList([],nop,_Procs,_Glob).


compileProcList([_-Pdef|Rest],Code,Procs,Glob) :-
	compileProcedure(Pdef,CodeP,Procs,Glob),
	compileProcList(Rest,CodeRest,Procs,Glob),
	Code = ( CodeP ; CodeRest).

% statement compilation predicate that is called after procedures have
% been collected into a dictionary
compileHL( (S1;S2), Code,Procs,GlobsIn,TGIn,GlobsOut,TGOut,
	   LocsIn,TLIn,LocsOut,TLOut,MaxTopIn,MaxTopOut,
	   Lev,RetLbl) :-
	compileStmt(S1,C1,Procs,GlobsIn,TGIn,GlobsAux,TGAux,
		    LocsIn,TLIn,LocsAux,TLAux,MaxTopIn,MaxTopAux,
		    Lev,RetLbl),
	compileHL(S2,C2,Procs,GlobsAux,TGAux,GlobsOut,TGOut,
		  LocsAux,TLAux,LocsOut,TLOut,MaxTopAux,MaxTopOut,
		  Lev,RetLbl),
	Code = ( C1 ; C2 ).


compileHL( (S;), Code,Procs,GlobsIn,TGIn,GlobsOut,TGOut,
	   LocsIn,TLIn,LocsOut,TLOut,MaxTopIn,MaxTopOut,
	   Lev,RetLbl) :-
	compileStmt(S,Code,Procs,GlobsIn,TGIn,GlobsOut,TGOut,
		    LocsIn,TLIn,LocsOut,TLOut,MaxTopIn,MaxTopOut,
		    Lev,RetLbl).

% compile procedures after being collected into a dictionary
compileProcedures(Procs,Code,Glob) :-
	assoc_to_list(Procs,PL),
	compileProcList(PL,Code,Procs,Glob).


% Main predicate:
%  - collect all procedures
%  - compile main program
%  - compile collected procedures
compileHLP( (P;Rest),Code,Pin,Pout,GlobsOut) :-
	isProcedure(P), !,
	procedureName(P,Pname),
	(   get_assoc(Pname,Pin,_)
	->  write('Duplicate procedure name:'), writeln(Pname), abort
	;   true ),
	put_assoc(Pname,Pin,P,Paux),
	compileHLP(Rest,Code,Paux,Pout,GlobsOut).


compileHLP(P,Code,Pin,_,GlobsOut) :-
	expandEnv([],EmptyEnv),
	compileHL(P,CodeP,Pin,EmptyEnv,-4,GlobsOut,_TGOut,
		  EmptyEnv,0,_LocsOut,_TLOut,0,MaxTopOut,
		  0,noreturn),
	compileProcedures(Pin,CProcs,GlobsOut), SP is 10000-MaxTopOut,
	C1 = ( framePtr = 10000 ; stackPtr = SP ),
	newlabel(End),
	C2 = ( goto End ),
	C3 = ( End ::),
	Code = ( C1 ; CodeP ; C2 ; CProcs ; C3 ).


% Compiler test
:- resetnewreg, resetnewlabel.

%% a = 200 gets affected. Therefore, the innermost
%% a becomes 201.

:- Program = (
		int a ; 
		a = 100 ;
		{
			int a ; 
			a = 200 ;
			{
				int a ;
				a = (^^a) + 1 ; 
			} ;
		} ;
	  ),
	empty_assoc(Empty),
	compileHLP(Program,Tac,Empty,_,Env1),
	writeTac(Tac),
	tacToObj(Tac,Obj),
	writeln('Translation into object code:'),
	writeObj(Obj,0),
	empty_assoc(Empty),
	execObj(0,Obj,Empty,Empty,_,HeapOut),
	writeln(HeapOut),
	writeln('Environment: '),
	writeln(Env1).


%% a = 100 gets affected. Therefore, the innermost
%% a becomes 101.
:- resetnewreg, resetnewlabel.

:- Program = (
		int a ; 
		a = 100 ;
		{
			int a ; 
			a = 200 ;
			{
				int a ;
				a = ^^(^^a) + 1 ; 
			} ;
		} ;
	  ),
	empty_assoc(Empty),
	compileHLP(Program,Tac,Empty,_,Env1),
	writeTac(Tac),
	tacToObj(Tac,Obj),
	writeln('Translation into object code:'),
	writeObj(Obj,0),
	empty_assoc(Empty),
	execObj(0,Obj,Empty,Empty,_,HeapOut),
	writeln(HeapOut),
	writeln('Environment: '),
	writeln(Env1).


:- resetnewreg, resetnewlabel.

:- Program = (
		int a ; 
		a = 100 ;
		{
			int a ; 
			a = 200 ;
			{
				int a ;
				a = ^^(^^(^^(^^a))) + 1 ; 
			} ;
		} ;
	  ),
	empty_assoc(Empty),
	compileHLP(Program,Tac,Empty,_,Env1),
	writeTac(Tac),
	tacToObj(Tac,Obj),
	writeln('Translation into object code:'),
	writeObj(Obj,0),
	empty_assoc(Empty),
	execObj(0,Obj,Empty,Empty,_,HeapOut),
	writeln(HeapOut),
	writeln('Environment: '),
	writeln(Env1).
