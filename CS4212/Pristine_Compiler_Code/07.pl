/*
Compiler for a C-style typed language. This time we no longer provide
an evaluator. Compared to 06.pl, the following operators are in effect.
     int : declares data of type int
     void: declares data of type void (only used for return values of
           procedures
     $   : pointer dereference -- $p equivalent to *p in C
     &	 : address-of operator -- same as in C
     @   : array access	-- a@[i] denotes the i-th element of array a
			   a@[i]@[j] denotes the (i,j)-th elem of matrix a
     ~~~ : structure definition operator -- str~~~{int x,y ; } ; defines
					    the structure 'str' with fields x,y
		  once structure 'str' has been defined, variables can be
		  declared as
		                str~~~ s1,s2 ; // two vars of type str
     ~~  : structure access operator --	s1~~x selects the 'x' field of s1

Type operators can be combined as in C. For instance

              int ($p@[10])#[int x, int y] ;

declares p as an array of pointers to functions that take two integer arguments
and return an integer.
*/

:- op(800,yfx,and).
:- op(810,yfx,or).
:- op(1099,yf,;).
:- op(960,fx,if).
:- op(959,xfx,then).
:- op(958,xfx,else).
:- op(960,fx,while).
:- op(959,xfx,do).
:- op(1050,fx,int).
:- op(1050,fx,void).
:- op(1050,xfx,~~~).  % structure definition operator
:- op(950,fx,goto).
:- op(950,xfx,goto).
:- op(950,fx,return).
:- op(970,xfy,::).
:- op(969,xf,::).
:- op(100,xfx,#).     % procedure call operator
:- op(110,fy,$).      % pointer dereference operator
:- op(105,yfx,@).     % array access operator
:- op(107,fx,&).      % address-of operator
:- op(115,yfx,~~).    % structure access operator
:- op(1070,xfx,(:::)).% procedure definition operator

/* Expression evaluation still needed by the TAC execution
   engine, so we leave it in. It is unchanged from 06.pl */

and(X,Y,Val) :- Val is abs(sign(X)) /\ abs(sign(Y)).
or(X,Y,Val) :- Val is abs(sign(X)) \/ abs(sign(Y)).
:- arithmetic_function(and/2).
:- arithmetic_function(or/2).

:- dynamic(ip/1).

evalExpr(X,Env,Env,_Procs,Val) :-
	atom(X),!, \+ atom_prefix(X,v_), getEnv(X,Env,Val),
	(   Val = undef
	->  write('Variable '), write(X), write(' uninitialized at IP:'),
	    retract(ip(IP)),writeln(IP), abort
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
	E =.. [F,A], member(F,[+,-,\]),!,
	evalExpr(A,EnvIn,EnvOut,Procs,ValA),
	C =.. [F,ValA], Val is C.

/* TAC execution engine is the same as in 06.pl */

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

execObj(IP,Code,Env,Heap,Env,Heap) :-
	retractall(ip(_)), assert(ip(IP)), max_assoc(Code,K,_), IP > K, !.
execObj(IP,Code,EnvIn,HeapIn,EnvOut,HeapOut) :-
	% retract(counter(Cnt)),Cnt1 is Cnt+1,assert(counter(Cnt1)),
	% (   Cnt1 < 10000 -> true ; writeln(HeapIn), abort ),
        get_assoc(IP,Code,Instr), empty_assoc(Empty),
	% writeln(HeapIn), writeln(EnvIn),
	% write(IP),write(::),writeln(Instr),%read(_),
        (   Instr = (goto L), !,
	    evalExpr(L,[EnvIn],_,Empty,IPnext),
	    EnvAux = EnvIn, HeapAux = HeapIn
        ;   Instr = (if E goto L), !,
	    evalExpr(E,[EnvIn],_,Empty,Val),
	    EnvAux = EnvIn, HeapAux = HeapIn,
	    (   Val = 1
            ->  evalExpr(L,[EnvIn],_,Empty,IPnext)
	    ;   IPnext is IP + 1 )
        ;   execTai(Instr,EnvIn,HeapIn,EnvAux,HeapAux), IPnext is IP+1 ),
        execObj(IPnext,Code,EnvAux,HeapAux,EnvOut,HeapOut).


/*
The environment interface is almost the same as in 06.pl.
Several unused predicates have been removed.
*/

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

createEnv(X,[Top|Rest],Val,[NewTop|Rest]) :-
	(   get_assoc(X,Top,_)
	->  write('Duplicate name in environment: '),
	    writeln(X), abort
	;   put_assoc(X,Top,Val,NewTop) ).

% expand an environment ; used when entering a new scope
%   -- the expansion is an association list added to the top of stack
%   -- the expansion can hold vars with same names as ones in enclosing
%      scopes without conflict.

expandEnv(Env,[Empty|Env]) :- empty_assoc(Empty).

/* To avoid a large number of parameters to the compiler predicates,
   we use a set of 'attributes'. The set of attributes is a dictionary
   where keywords are associated to values. The set of attributes is
   used as a single parameter to the compiler predicates. In the rules,
   the attributes are retrieved from the set via the API given below
*/

% retrieve a list of attributes from the set
%   getAttr([keyword1:Val1,keyword2:Val2],Attr) binds the variables
% Val1 and Val2 to the values associated with the respective
% attributes in the attribute set Attr. Val1 and Val2 must be unbound
% at the time of the call.
getAttr([],_) :- !.
getAttr([X:Y|T],Attrs) :- member((X:Y),Attrs), !, getAttr(T,Attrs).

% update (or create) a list of attributes in a set
%    updAttr([keyword1:Val1,keyword2:Val2],AttrIn,AttrOut)
%    replaces the values Val1 and Val2 for the respective
%    keywords in AttrIn. The resulting attribute set is
%    bound to AttrOut. Attributes that are not mentioned
%    in the list passed as the first argument remain unchanged
%    and are simply transferred from AttrIn to AttrOut.
updAttr(NewFeatures,[],NewFeatures) :- !.
updAttr(NewFeatures,[X:_|Attrs],[X:Y|NewAttrs]) :-
	member((X:Y),NewFeatures), !,
	delete(NewFeatures,(X:Y),NewNewFeatures),
	updAttr(NewNewFeatures,Attrs,NewAttrs).
updAttr(NewFeatures,[X:Y|Attrs],[X:Y|NewAttrs]) :-
	updAttr(NewFeatures,Attrs,NewAttrs).

/* The code for the compiler starts here. There are many helper
   predicates, mostly to handle declarations and perform type
   checking.

   To avoid a large number of arguments, the data computed by
   the compiler and passed along the syntax tree is now stored
   in an attribute set. Most of the helper predicates need
   this information, so the 'current' attribute set is passed
   as an argument to them. The attributes relevant to the current
   computation are retrieved from the attribute set via a keyword.
   For a list of keywords, and their meaning, check the 'compile'
   predicate defined below.
*/

% Check if a structure definition exists in the relevant environment.
% If the scope level is 0, then the name is assumed to be defined
% in the global structure table. Otherwise, the local structure table
% is searched first, and then the global structure table.
% These tables are environments, and are stored in the current set
% of Attributes; they must  retrieved first,
% before the check can be performed.
checkStructure(Name,Attrs) :-
	getAttr([level:L,globalStruct:GS,localStruct:LS],Attrs),
	(   L == 0, varInEnv(Name,GS), !
	;   L \= 0, ( varInEnv(Name,LS),! ; varInEnv(Name,GS)),!
        ;   write('Undeclared structure:'), writeln(Name), abort ).

% Process argument types for a procedure's list of arguments.
% Arg 1 (in) : list of arguments as they appear in the procedure's def
% Arg 2 (in) : current set of attributes
% Arg 3 (out): list of argument names
% Arg 4 (out): list of argument types, in same order
%   -- uses processType(Decl,Attr,Id,Type), defined below
processArgsTypes([],_,[],[]) :- !.
processArgsTypes([DH|DT],Attrs,[IH|IT],[TH|TT]) :-
	processType(DH,Attrs,IH,TH), processArgsTypes(DT,Attrs,IT,TT).

% helper for processType(Decl,Attr,Id,Type)
processTypeHlp(X,_Attrs,X,TypeAccum,TypeAccum) :-
	atom(X),!.
processTypeHlp(($D),Attrs,I,TypeAccum,Type) :- !,
	processTypeHlp(D,Attrs,I,($TypeAccum),Type).
processTypeHlp((D@[Dim]),Attrs,I,TypeAccum,Type) :- !,
	integer(Dim),
	processTypeHlp(D,Attrs,I,(TypeAccum@[Dim]),Type).
processTypeHlp((D#Args),Attrs,I,TypeAccum,Type) :- !,
	processArgsTypes(Args,Attrs,_Ids,Types),
	processTypeHlp(D,Attrs,I,(TypeAccum#Types),Type).

% Process a declaration and extract its type and id
% The representation of the type is an inside-out tree
% as compared to the declaration. For instance, for the
% declaration
%              int $($p@[10])#[int x] ;
% (that is, p is an array of 10 elem of pointers to a
%  procedure that takes in one integer argument and
%  returns a pointer to int), we have the type representation
%
%	      ($($int)#[int])@[10]
%
%  This representation makes it more obvious that the type of
%  p really is. In this case, p is an array, since the
%  outmost operator of the type is '@'. The type of the array's
%  elements can be derived from the left operand of '@'.
processType((int Decl),Attrs,Id,Type) :- !,
	processTypeHlp(Decl,Attrs,Id,int,Type).
processType((void Decl#Args),Attrs,Id,Type) :- !,
	processArgsTypes(Args,Attrs,_Ids,Types),
	processTypeHlp(Decl,Attrs,Id,((void)#(Types)),Type).
processType((void _),_,_,_) :- !,
	writeln('Attempt to declare a non-function as void'), abort.
processType((Name~~~ Decl),Attrs,Id,Type) :-
	(   checkStructure(Name,Attrs), !,
	    processTypeHlp(Decl,Attrs,Id,(~~~(Name)),Type)
	;   write('Undeclared structure:'), writeln(Name), abort ).

% extract the list of arguments from a procedure prototype
% declaration.
% Arg1 (in) : prototype declaration
% Arg2 (out): list of arguments in the prototype
%
% For instance, the declaration
%     int $$p#[int x,int y]
% declares a procedure with list of arguments [int x,int y]
getArgList((X#L),L) :- atom(X), !.
getArgList((X#_),L) :- !, getArgList(X,L).
getArgList((int X),L) :- !, getArgList(X,L).
getArgList((void X),L) :- !, getArgList(X,L).
getArgList(($ X),L) :- !, getArgList(X,L).
getArgList((X @ _),L) :- !, getArgList(X,L).
getArgList((_ ~~~ X),L) :- !, getArgList(X,L).

% predicate to compute the size of a given type.
% useful in allocating space for a variable
%   -- size of int and pointer is 4
%   -- size of a struct is sum of sizes of fields
%   -- size of array: #elem * sizeof(elem)
%   -- size of procedure is undefined, raise error if attempted
% Arg1 (in) : type whose size is sought
% Arg2 (in) : current attributes (to access defined structures)
% Arg3 (out): size of given type, in bytes
sizeOf((int),_Attrs,4) :- !.
sizeOf((T@[I]),Attrs,Size) :- !, sizeOf(T,Attrs,TS), Size is TS*I.
sizeOf(($_),_,4) :- !.
sizeOf((D#_),_Attrs,-1) :- !,
	write('Attempted to compute size of procedure:'), writeln(D), abort.
sizeOf((void X),_Attrs,-1) :- !,
	write('Attempted to compute size of procedure:'), writeln(X), abort.
sizeOf(((~~~(N))),Attrs,Size) :-
	getAttr([level:Lev,globalStruct:GS,localStruct:LS],Attrs),
	(   Lev == 0
	->  getEnv(N,GS,SDef), getAttr([lTopAddr:SSize],SDef)
	;   (	varInEnv(N,LS)
	    ->	getEnv(N,LS,SDef), getAttr([lTopAddr:SSize],SDef)
	    ;	getEnv(N,GS,SDef), getAttr([lTopAddr:SSize],SDef) ) ),
	Size is SSize.

% helper for 'sameType'
sameTypeArgs([],[]) :- !.
sameTypeArgs([H1|T1],[H2|T2]) :- !,
	sameType(H1,H2), sameTypeArgs(T1,T2).

% predicate to check if two types are the same.
% useful in type checking, for instance to check if the
% type of an actual parameter is the same as the corresponding
% formal argument in a procedure's definition
% There are some quirks, due to implicit type conversions, namely
% that the type of an array of elements of type T is the same
% as the type of a pointer to T. The last 4 rules cater to these
% implicit type conversions.
sameType(int,int) :- !.
sameType(void,void) :- !.
sameType((~~~(X)),(~~~(X))) :- !.
sameType(T1#Args1,T2#Args2) :- !,
	sameType(T1,T2), sameTypeArgs(Args1,Args2).
sameType(($T1),($T2)) :- !, sameType(T1,T2).
sameType(($T1),(T2@_)) :- !, sameType(T1,T2).
sameType((T1@_),($T2)) :- !, sameType(T1,T2).
sameType((T1@_),(T2@_)) :- !, sameType(T1,T2).

% predicate that helps with type checking. If the
% current type and the expected type are not the same,
% this predicate raises an error message.
checkTypeError(Symbol,Type,ExpectedType) :-
	sameType(Type,ExpectedType),!
	; write('Invalid type for symbol:'),
	write(Symbol), write(' ; expected: '),
	write(ExpectedType),
	write(' , got: '), writeln(Type), abort.

% helper predicate to handle code concatenation.
% declarations produce no executable code. The compiler will produce
% 'nocode' for such declarations. This keyword will dissapear in the
% process of code concatenation.

appendCode(nocode,X,X) :- !.
appendCode(X,nocode,X) :- !.
appendCode(X,Y,(X;Y)).
appendCode([C],C) :- !.
appendCode([H|T],C) :- appendCode(T,CT),appendCode(H,CT,C).

:- dynamic auxreg/1.
:- dynamic auxvar/1.

% generate new register names - same as 06.pl
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

% helper for 'instruction'
% isolates the first instruction in a sequence of TAC instructions,
% and places a label on it.
addLabel((C1;C2),L,(LC1;C2)) :- !,addLabel(C1,L,LC1).
addLabel(C,L,(L::C)).

% predicate to tackle label requests.
% If the current attribute set has a label request (that is,
% different from 'nolabel'), then place this label in front of
% the first instruction given as the third argument.
instruction(C,Attr,C) :- getAttr([labelRequest:nolabel],Attr), !.
instruction(LC,Attr,C) :-
	getAttr([labelRequest:L],Attr),
	addLabel(C,L,LC).

% predicate that forces a label request. If there
% already exists a label request in the current attribute
% set, then that label request is kept. Otherwise, a new label
% request is created. Useful to handle the situation where, after
% generating some TAC code, call it "C", we may end up with an
% attribute set containing a label request, call it "L". Now,
% the TAC instruction immediately following "C" must have a label,
% due to some other factors. The typical approach is to check if
% there exists a proper label request after "C" has been
% generated (that is, check if L == nolabel), and if there is,
% simply use the existing label, otherwise generate a new one.
forceLabel(Label,AttrIn,AttrOut) :-
	getAttr([labelRequest:L],AttrIn),
	(   L == nolabel
	->  newlabel(Label),
	    updAttr([labelRequest:Label],AttrIn,AttrOut)
	;   Label = L ).

% Predicate to compile expressions. Besides the handling of
% the new type operators, there are a few optimizations for
% generating more efficient code.
%
% Arg1 (in) : expression to be compiled
% Arg2 (Out): generated code
% Arg3 (Out): TAC expression that denotes the result
%	      of original HL expression after execution
%	      of generated TAC code (Arg2)
%             -- this is a major change from 06.pl
%                now, the expression "x" may be compiled
%                into "nocode", with the result denoted
%                by "[framePtr+8]", for instance. By allowing
%                more complex expressions to denote the result,
%                the code size can be reduced, and some redundant
%                register transfers eliminated
% Arg4 (out): type of result
% Arg5 (in) : input attribute set, used to avoid too many input
%             arguments
% Arg6 (out): output argument set
compileExpr(X,nocode,X,(int),Attr,Attr) :-
	integer(X), !.
compileExpr(X,C,R,Type,Attrs,Attrs) :-
	% symbol "X" is retrieved from corresponding environment
	% (dependent on level), and the address where the
	% value of symbol should be stored is computed
	% If the symbol is global, then the address is absolute,
	% else the address is an offset wrt framePtr. If the offset
	% happens to be 0, then it is discarded. The code tries to
	% make the result expression as simple as possible.
	% The variable is also checked in the type table, and if
	% not found, an error is raised. If found, (meaning that a
	% declaration of this symbol has been previously encountered
	% the type is returned in "Type".
	atom(X), !,
	getAttr( [ level:Lev,globalAddr:GAddrs,globalType:GTypes,
		   localAddr:LAddrs,localType:LTypes ], Attrs),
	(   Lev == 0
	->  getEnv(X,GTypes,Type), getEnv(X,GAddrs,Addr)
	;   (   varInEnv(X,LTypes)
	    ->	getEnv(X,LTypes,Type), getEnv(X,LAddrs,O),
		(   O >= 0
		->  sizeOf(Type,Attrs,S), Off is O+S, Addr = framePtr - Off
		;   Off is - O, Addr = framePtr + Off )
	    ;	getEnv(X,GTypes,Type), getEnv(X,GAddrs,Addr) ) ),
	% we evaluate objects that can are guaranteed to
	% fit in a register to their value
	% and we evaluate objects that are not guaranteed to fit in
	% a register (such as arrays and structures) to their address
	(   Type = _ # _
        ->  R = X, C = nocode
	;   (   ( Type = _ @ _, ! ; Type = ~~~(_) )
	    ->  R = Addr, C = nocode % result is address of object
	    ;   R = [Addr], C = nocode ) ). % result is value of object
compileExpr(E,Code,R,Tx,AttrIn,AttrOut) :-
	% this rule handles pointer arithmetic
	% Pointer + Integer evaluates to pointer that references
	% an address at a distance of sizeof($Pointer)*Integer away.
	% The calculation for Pointer-Integer is similar.
	E =.. [F,X,Y], member(F,[+,-]),
	compileExpr(X,Cx,Rx,Tx,AttrIn,Attr1),
	sameType(Tx,($ _)),
	compileExpr(Y,Cy,Ry,Ty,Attr1,Attr2),
	sameType(Ty,int), !,
	(   Tx = ($ T), ! ; Tx = ( T @ _ )  ),
	sizeOf(T,Attr2,ElemSize),
	% The return expressions Rx and Ry are inspected in order
	% to optimize the generated code. New registers and new
	% instructions are generated only when absolutely necessary
	(   integer(Ry)
	->  Offset is Ry*ElemSize,
	    (	integer(Rx)
	    ->	Op =.. [F,Rx,Offset], R is Op, C = nocode, AttrOut = Attr2
	    ;	(   atom(Rx)
		->  Op =.. [F,Rx,Offset], CC = ( Rx = Op ), R = Rx
		;   newreg(R), Op =.. [F,R,Offset], CC = ( R = Rx ; R = Op )
		),
		instruction(C,Attr2,CC), updAttr([labelRequest:nolabel],Attr2,AttrOut) )
	;   (   atom(Ry)
	    ->  newreg(Roff), Coff = ( Roff = Ry*ElemSize ),
		(	( atom(Rx),! ; integer(Rx) )
		->	C = (Coff ; Roff = Rx + Roff), R = Roff
		;	newreg(Rbase), C = (Rbase = Rx ; Coff ; Rbase = Rbase + Roff), R = Rbase )
	    ;   newreg(Roff), newreg(R),
		C = ( R=Rx, Roff = Ry, Roff = Roff*ElemSize, R=R+Roff ) ),
	    instruction(C,Attr2,CC), updAttr([labelRequest:nolabel],Attr2,AttrOut) ),
	appendCode([Cx,Cy,C],Code).
compileExpr(E,Code,R,(int),AttrIn,AttrOut) :-
	% This rule handles arithmetic expressions
	% The optimizations are similar to the ones in the
	% previous rule
	E =.. [F,X,Y],
	member(F,[+,-,*,/,mod,<<,>>,/\,\/,and,or,xor]),!,
	compileExpr(X,Cx,Rx,Tx,AttrIn,AttrAux),
	compileExpr(Y,Cy,Ry,Ty,AttrAux,AttrY),
	checkTypeError(X,Tx,int), checkTypeError(Y,Ty,int),
	(   integer(Rx)
	->  (   integer(Ry)
	    ->	Op =.. [F,Rx,Ry], R is Op, AttrIn = AttrOut, C = nocode
	    ;	(   atom(Ry)
		->  Op =.. [F,Rx,Ry], CC = (Ry = Op), R = Ry
		;   newreg(R), Op =.. [F,Rx,R], CC = ( R = Ry ; R = Op )
		) ,
		instruction(C,AttrY,CC),
		updAttr([labelRequest:nolabel],AttrY,AttrOut) )
	;   (   atom(Rx)
	    ->	(   ( integer(Ry),! ; atom(Ry) )
		->  Op =.. [F,Rx,Ry], CC = (Rx = Op), R = Rx
		;   newreg(R), Op =.. [F,Rx,R], CC = ( R = Ry ; R = Op ) )
	    ;	newreg(R), newreg(Raux), Op =.. [F,R,Raux],
		CC = (R = Rx ; Raux = Ry ; R = Op) ),
	    instruction(C,AttrY,CC),
	    updAttr([labelRequest:nolabel],AttrY,AttrOut) ),
	appendCode([Cx,Cy,C],Code).
compileExpr(E,Code,Result,(int),AttrIn,AttrOut) :-
	% implementation of relational operators is similar
	% to the one in 06.pl (except for the use of attributes and
	% type checking)
	E =.. [F,A,B], member(F,[<,>,=<,>=,==,\=]), !,
	(   B == 0
	->  compileExpr(A,CodeAB,R,TA,AttrIn,AttrAux),
	    checkTypeError(A,TA,int)
	;   compileExpr(A-B,CodeAB,R,TAB,AttrIn,AttrAux),
	    checkTypeError(A-B,TAB,int) ),
	Op =.. [F,R,0], newreg(Result),
	newlabel(LblOut), newlabel(Skip),
	instruction(C,AttrAux, (  if Op goto Skip ; Result = 0 ;
			         goto LblOut ; Skip::Result = 1) ),
	updAttr([labelRequest:LblOut],AttrAux,AttrOut),
	appendCode(CodeAB,C,Code).
compileExpr((X ? Y : Z),Code,R,Ty,AttrIn,AttrOut) :- !,
	% similar to 06.pl, but uses attributes and performs
	% type checking
        newreg(R), newreg(Rx),newlabel(Skip), newlabel(Lout),
        compileExpr(X,Cx,Qx,Tx,AttrIn,Attr1),
	checkTypeError(X,Tx,int),
	updAttr([labelRequest:nolabel],Attr1,Attr2),
        compileExpr(Y,Cy,Qy,Ty,Attr2,Attr3),
	(   [Py] = Qy, ! ; Py = Qy ),
	updAttr([labelRequest:Skip],Attr3,Attr4),
        compileExpr(Z,Cz,Qz,Tz,Attr4,Attr5),
	(   [Pz] = Qz, ! ; Pz = Qz ),
	sameType(Ty,Tz),
	updAttr([labelRequest:Lout],Attr5,AttrOut),
	instruction(C1,Attr1,(Rx = Qx ; if Rx == 0 goto Skip)),
	instruction(C2,Attr3,(R = Qy ; goto Lout)),
	instruction(C3,Attr5,(R = Qz)),
        appendCode([Cx,C1,Cy,C2,Cz,C3],Code).
compileExpr(E,Code,R,(int),AttrIn,AttrOut) :-
	% unary operators + and -
	E =.. [F,A], member(F,[+,-]), !,
	C =.. [F,0,A], compileExpr(C,Code,R,(int),AttrIn,AttrOut).
compileExpr(\ X,Code,R,(int),AttrIn,AttrOut) :- !,
	% unary bitwise negation
	compileExpr((-1) xor X, Code, R, (int),AttrIn,AttrOut).
compileExpr(($ E),Code,R,Type,AttrIn,AttrOut) :- !,
	% pointer dereferencing, pay attention to the computation
	% of the result type. Also notice the optimization
	% of the generated code
	% The return type may either be a scalar (fit in a register)
	% or a non-scalar (not guaranteed to fit in a register).
	% For scalars, the value is returned. For non-scalars,
	% their address is returned as result
	compileExpr(E,CE,RE,TE,AttrIn,AttrAux),
	checkTypeError(E,TE,($ _)),
	(   TE = ($ Type),! ; TE = (Type @ _) ),
	(   RE =.. [_]
	->  (   ( Type = ( _ @ _),!;Type=(_#_) )
	    ->  R = RE
	    ;   R = [RE]),
	    Code = CE, AttrOut = AttrAux
	;   (   ( integer(RE), ! ; atom(RE) )
	    ->	(  (Type = ( _ @ _ ),!;Type=(_#_) )
		-> R = RE
		;  R = [RE] ),
		Code = CE, AttrOut = AttrAux
	    ;	newreg(Reg),
		(  ( Type = ( _ @ _),!;Type=(_#_) )
		-> R = Reg
		;  R = [Reg]),
		instruction(C,AttrAux,(Reg=RE)),
		updAttr([labelRequest:nolabel],AttrAux,AttrOut),
		appendCode(CE,C,Code) ) ).
compileExpr((X@[Y]),Code,Res,T,AttrIn,AttrOut) :-
	% array access; similar to pointer dereferencing
	compileExpr(X,Cx,Rx,Tx,AttrIn,Attr1),
	compileExpr(Y,Cy,Ry,Ty,Attr1,Attr2),
	sameType(Tx,(_ @ _)), sameType(Ty,int), !,
	(   Tx = ($ T),! ; Tx = ( T @ _ )  ),
	sizeOf(T,Attr2,ElemSize),
	(   integer(Rx), integer(Ry),!,
	    R is Rx+Ry*ElemSize,
	    AttrOut=Attr2, C = nocode
	;   integer(Rx), atom(Ry), !,
	    instruction(C,Attr2, (  Ry = Ry*ElemSize ; Ry = Rx + Ry )), R = Ry,
	    updAttr([labelRequest:nolabel],Attr2,AttrOut)
	;   integer(Rx), !, newreg(R),
	    instruction(C,Attr2, ( R = Ry ; R = R*ElemSize ; R = Rx + R ) ),
	    updAttr([labelRequest:nolabel],Attr2,AttrOut)
	;   atom(Rx), integer(Ry), ! ,
	    Offset is Ry*ElemSize,
	    instruction(C,Attr2, ( Rx = Rx + Offset ) ), R = Rx,
	    updAttr([labelRequest:nolabel],Attr2,AttrOut)
	;   atom(Rx), atom(Ry), ! ,
	    instruction(C,Attr2, ( Ry = Ry * ElemSize ; Rx = Rx + Ry ) ), R = Rx,
	    updAttr([labelRequest:nolabel],Attr2,AttrOut)
	;   atom(Rx), !, newreg(R),
	    instruction(C,Attr2, ( R = Ry ; R = R * ElemSize ; R = Rx + R )),
	    updAttr([labelRequest:nolabel],Attr2,AttrOut)
	;   integer(Ry), !, newreg(R), Offset is Ry * ElemSize,
	    instruction(C,Attr2, ( R = Rx ; R = R+Offset ) ),
	    updAttr([labelRequest:nolabel],Attr2,AttrOut)
	;   atom(Ry), !, newreg(R),
	    instruction(C,Attr2, ( R = Rx ; Ry = Ry*ElemSize ; R = R+Ry ) ),
	    updAttr([labelRequest:nolabel],Attr2,AttrOut)
	;   newreg(R), newreg(Raux),
	    instruction(C,Attr2, ( R = Rx ; Raux = Ry ; Raux = Raux * ElemSize ; R = R + Raux ) ),
	    updAttr([labelRequest:nolabel],Attr2,AttrOut)
	),
	(   ( T = (_ @ _),! ; T = ~~~(_) ) -> Res = R ; Res = [R]  ),
	appendCode([Cx,Cy,C],Code).
compileExpr( ( & X ), Code,R,T,AttrIn,AttrOut ) :-
	% address-of operator, can only be applied
	% when the result of X is of the form [R]
	compileExpr(X,Code,RA,TA,AttrIn,AttrOut),
	(   RA = [R], T = $ TA, !
	;   RA = R, (TA = _ @ _,!; TA = ~~~(_),!; TA = _#_), T = $ TA ),! ;
	write(' The & operator must be applied to lvalue in expression: '),
	writeln(X).
compileExpr( (X~~Y),Code,R,T,AttrIn,AttrOut ) :- !,
	% structure access. The type of X must be a
	% declared structure, and Y must be the name
	% of a field in the structure. Y is interpreted
	% as an offset that needs to be added to the
	% address of X
	compileExpr(X,Cx,Rx,Tx,AttrIn,AttrAux),
	(   Tx = ~~~(Str)
	->  true
	;   write('The ~~ operator must have a structure on left side'),
	    writeln(X~~Y), abort ),
	getAttr([level:Lev,globalStruct:GS,localStruct:LS],AttrAux),
	(   Lev == 0
	->  getEnv(Str,GS,SDef)
	;   (	varInEnv(Str,LS)
	    ->	getEnv(Str,LS,SDef)
	    ;	getEnv(Str,GS,SDef) ) ),
        getAttr([localAddr:FieldOffsets,localType:FieldTypes],SDef),
	getEnv(Y,FieldTypes,T), getEnv(Y,FieldOffsets,Offset),
	% optimization of resulting code
	(   ( integer(Rx), !, Addr is Rx+Offset ;
	      Offset == 0, Addr = Rx ),!,
	    AttrAux = AttrOut, C = nocode
	;   atom(Rx), !, Addr = Rx,
	    instruction(C,AttrAux, ( Rx = Rx + Offset ) ),
	    updAttr([labelRequest:nolabel],AttrAux,AttrOut)
	;   (    Rx = Base + A, !, O is A + Offset,
	         Addr = Base+O, C = nocode
	    ;    Rx = Base - A,!, A > Offset,
		 O is A - Offset, Addr = Base-O, C = nocode
	    ;   newreg(Addr),
		instruction(C,AttrAux,(Addr = Rx ; Addr = Addr+Offset))),
	    updAttr([labelRequest:nolabel],AttrAux,AttrOut) ),
	(   ( T = _ @ _, ! ; T = ~~~(_) )
	->  R = Addr, Cz = nocode % return address of non-scalar
	;   (   Addr = [_]
	    ->	newreg(Tmp), Cz = ( Tmp = Addr ), R = Tmp
	    ;	Cz = nocode, R = [Addr] ) ), % return value of scalar
	appendCode([Cx,C,Cz],Code).
compileExpr( (P#Args),Code,returnResult,T,AttrIn,AttrOut) :- !,
	% Procedure call, return in global register "returnResult"
	% P may be an expression (e.g dereferencing of pointer to procedure)
	compileExpr(P,CP,RP,TP,AttrIn,AttrP),
	% Create new local scope and add bindings to arguments
	% Generated code pushes results of arguments on stack
	compileArgs(Args,CA,TArgs,AttrP,AttrArgs,StackSpace),
	(   TP = T # TArgs % type check that call is correct
	->  true
	;   write('Invalid type in procedure call: '), writeln(P#Args) ),
	% force a return label, even if CA does not request one
	forceLabel(Label,AttrArgs,AttrLabel), newreg(RetAddr),
	% code for saving return address on stack
	instruction(C,AttrArgs, (          stackPtr = stackPtr - 4 ;
					   RetAddr = Label ;
				           [stackPtr] = RetAddr  ;
		                           goto RP ;
				  Label :: stackPtr = stackPtr + StackSpace) ),
	updAttr([labelRequest:nolabel],AttrLabel,AttrOut),
	appendCode([CP,CA,C],Code).

% compilation of procedure arguments
% -- also computes the amount of stack shrinking space for
%    clearing the arguments off the stack
% -- generates code for computing each argument and pushing
%    it on the stack
compileArgs([],nocode,[],Attr,Attr,0):-!.
compileArgs([H|T],C,[TH|TT],AttrIn,AttrOut,SS) :-
	(   compileExpr(H,CH,RH,TH,AttrIn,AttrH)
	->  true
	;   write('Possible typing error: '),
	    write(H), write(' is expected to have type '),
	    writeln(TH), abort ),
	instruction(CS1,AttrH,(stackPtr=stackPtr-4)),
	(   ( atom(RH),! ; integer(RH) )
	->  CS2 = nocode, Reg = RH
	;   newreg(Reg), CS2 = ( Reg = RH ) ),
	appendCode([CH,CS1,CS2,([stackPtr]=Reg)],CS),
	updAttr([labelRequest:nolabel],AttrH,AttrT),
	compileArgs(T,CT,TT,AttrT,AttrOut,ST),
	appendCode(CS,CT,C), SS is ST+4.

% compile statement with no semicolon
% Arg1 (in) : HL statement to be compiled
% Arg2 (out): generated TAC code
% Arg3 (in) : input attribute set
% Arg4 (out): output attribute set
compileStmtNS((LHS=RHS),Code,AttrIn,AttrOut) :- !,
	% assignment:
	%  -- check that types of LHS and RHS are compatible
	%  -- optimize the transfer operation, avoiding redundant instructions
	compileExpr(LHS,CL,RL,TL,AttrIn,AttrL),
	compileExpr(RHS,CR,RR,TR,AttrL,AttrR),
	(   sameType(TL,TR)
	->  true
	;   write('Incompatible types in assignment:'), write((LHS=RHS)),
	    write(' '),writeln((TL\=TR)), abort ),
	(   RL = [_]
	->  true
	;   write('Invalid lvalue in assignment:'),
	    writeln((LHS=RHS)), abort ),
	(   RR =.. [_]
	->  Reg = RR, C1 = nocode
	;   newreg(Reg), C1 = (Reg = RR) ),
	appendCode(C1,(RL = Reg),C2),
	instruction(C,AttrR,C2),
	updAttr([labelRequest:nolabel],AttrR,AttrOut),
	appendCode([CL,CR,C],Code).
compileStmtNS((return X),Code,AttrIn,AttrOut) :- !,
	% return statement implemented as
	%  -- check type compatibility
	%  -- store value of X in global register returnResult
	%  -- jump to return label, which is retrieved from the attribute set
	compileExpr(X,CX,RX,TX,AttrIn,AttrX),
	getAttr([globalType:GT,returnLabel:RetLbl,curProc:ProcName],AttrX),
	getEnv(ProcName,GT,(TP#_)),
	(   sameType(TX,TP)
	->  true
	;   write('Invalid return type: expression '), write(X),
	    write(' is of type '), writeln(TX),
	    write(' but expected to be of type '),
	    writeln(TP), abort ),
	instruction(C,AttrX, ( returnResult = RX ; goto RetLbl ; ) ),
	appendCode(CX,C,Code), updAttr([labelRequest:nolabel],AttrX,AttrOut).
compileStmtNS(return,Code,AttrIn,AttrOut) :- !,
	% return of void value, similar to previous rule, no need to
	% set global register
	getAttr([globalType:GT,returnLabel:RetLbl,curProc:ProcName],AttrIn),
	getEnv(ProcName,GT,(TP#_)),
	(   sameType(TP,void)
	->  true
	;   write('Invalid return type: expected '), write(TP),
	    write(' but got void.'), abort ),
	instruction(Code,AttrIn, ( goto RetLbl ; ) ),
	updAttr([labelRequest:nolabel],AttrIn,AttrOut).
compileStmtNS({S},Code,AttrIn,AttrOut) :- !,
	% add a frame to the current environment and compile S
	% new declarations in S will be allocated in the new frame
	% upon return from recursive compilation, discard newly added frame
	getAttr([localAddr:LA,localType:LT,localStruct:LS,level:Lvl],AttrIn),
	expandEnv(LA,NewLA), expandEnv(LT,NewLT), expandEnv(LS,NewLS),
	updAttr([localAddr:NewLA,localType:NewLT,
		 localStruct:NewLS,level:1],AttrIn,AttrAux1),
	compileGlobalDecl(S,AttrAux1,AttrAux2,RestS),
	compileStmt(RestS,Code,AttrAux2,AttrAux3),
	updAttr([localAddr:LA,localType:LT,
		 localStruct:LS,level:Lvl],AttrAux3,AttrOut).

% compilation of statements, similar to 06.pl, but using attribute sets
compileStmt((S1;S2),Code,AttrIn,AttrOut) :- !,
	compileStmtNS(S1,C1,AttrIn,AttrAux),
	compileStmt(S2,C2,AttrAux,AttrOut),
	appendCode(C1,C2,Code).
compileStmt((S;),Code,AttrIn,AttrOut) :-
	compileStmtNS(S,Code,AttrIn,AttrOut).

/*
Compilation of declarations will extract identifiers and
their types, and will compute the address of each identifier
and save it in an attribute set; this information will be made
available to the compilation of expressions
*/

% compilation of a structure, the new structure is added
% to either the table 'globalStruct' or 'localStruct', depending
% on the scoping level
% The definition of each structure is encoded as a new local scope
% The key 'maxStack' will map into the size of the structure
compileStruct((Name~~~{Decls}),AttrsIn,AttrsOut) :-
	getAttr([level:Lev,globalStruct:GS,localStruct:LS,
		 globalType:GT],AttrsIn),
	(   Lev == 0 -> S = GS ; S = LS ),
        expandEnv([],E),
        updAttr([level:1,localAddr:E,lTopAddr:0,localType:E,
		 globalType:GT,globalStruct:GS,
		 localStruct:LS,maxStack:0],[],AI),
	compileStructDecl(Decls,AI,AO),
	createEnv(Name,S,AO,NewS),
	(   Lev == 0
	->  updAttr([globalStruct:NewS], AttrsIn,AttrsOut)
	;   updAttr([localStruct:NewS], AttrsIn,AttrsOut) ).

% helper predicate that iterates through declarations inside
% a structure
compileStructDecl((Decl;Rest),AI,AO) :- !,
	compileDecl(Decl,AI,Aaux),
	compileStructDecl(Rest,Aaux,AO).
compileStructDecl((Decl;),AI,AO) :- compileDecl(Decl,AI,AO).

% helper predicate that iterates through a sequence of declarations
% The "Term" is a simple declaration (no comma separated type
% expressions). For each term, we extract the symbol, the type,
% and compute the symbol's address. All this information is added
% to the current attribute set, in the relevant attributes.
compileDeclHelper(Term,AttrsIn,AttrsOut) :-
	getAttr([level:Lev],AttrsIn),
	(   Lev == 0
	->  getAttr([globalAddr:Addrs,gTopAddr:TopAddr,
		     globalType:Types], AttrsIn)
	;   getAttr([localAddr:Addrs,lTopAddr:TopAddr,
		     localType:Types,maxStack:M], AttrsIn) ),
        processType(Term,AttrsIn,Id,Type),
	(   varInEnv(Id,Types)
	->  write('Identifier already declared: '), writeln(Id), abort
	;   true ),
	createEnv(Id,Types,Type,NewTypes),
	(   Type = (_#_)
	->   NewAddrs = Addrs, NewTopAddr = TopAddr
	;    sizeOf(Type,AttrsIn,Size),
	     createEnv(Id,Addrs,TopAddr,NewAddrs),
	     NewTopAddr is TopAddr+Size ),
	(   Lev == 0
	->  updAttr([globalAddr:NewAddrs,gTopAddr:NewTopAddr,
		     globalType:NewTypes], AttrsIn,AttrsOut)
	;   NewM is max(NewTopAddr,M),
	    updAttr([localAddr:NewAddrs,lTopAddr:NewTopAddr,
		     localType:NewTypes,maxStack:NewM], AttrsIn,AttrsOut) ).

% compile a full declaration. If the declaration has comma separated
% type expressions, turn this expressions into simple declarations
% (no commas), and invoke the helper
compileDecl(D,AttrsIn,AttrsOut) :-
	(   D = (int (S1,S2)), Head = (int S1), Tail = (int S2)
	;   D = (void (S1,S2)), Head = (void S1), Tail = (void S2)
	;   D = (Name~~~(S1,S2)), Head = (Name~~~S1), Tail = (Name~~~S2) ),!,
	compileDeclHelper(Head,AttrsIn,AttrsAux),
	compileDecl(Tail,AttrsAux,AttrsOut).
compileDecl(D,AttrsIn,AttrsOut) :- compileDeclHelper(D,AttrsIn,AttrsOut).

% Predicate to dispatch betweeen compilation of structure and
% non-structure declarations
compileGlobalDeclHelper(D,AttrsIn,AttrsOut) :-
	(   D = ( _ ~~~ { _ } )
	->  compileStruct(D,AttrsIn,AttrsOut)
	;   D =.. [F|_], member(F,[int,void,~~~]),
	    compileDecl(D,AttrsIn,AttrsOut) ).

% compilation of global declarations
% invoked before compilation of main program
% -- detects the beginning of main program, and returns
%    control to main compilation predicate
compileGlobalDecl((D;Rest),AttrsIn,AttrsOut,RestProg) :-
	compileGlobalDeclHelper(D,AttrsIn,AttrsAux),!,
	compileGlobalDecl(Rest,AttrsAux,AttrsOut,RestProg).
compileGlobalDecl(D,AttrsIn,AttrsOut,_) :-
	compileGlobalDeclHelper(D,AttrsIn,AttrsOut),!.
compileGlobalDecl(Rest,Attrs,Attrs,Rest).

/*
Compilation of procedures is performed in two phases.
In phase one (before compiling the main program), procedure prototypes
are extracted from the procedure definitions and saved into the
attribute set, so that procedure calls can be type checked throughout
the process of compiling the main program. Phase 2 occurs after the
compilation of the main program; in this phase, the procedures are
compiled into TAC.
*/

/* helper for phase 1: processes 1 procedure and saves
   the information in the current attribute set */
collectProcsHelper((Decl:::Body),AttrsIn,AttrsOut) :-
	getAttr([level:Lev],AttrsIn),
	(   Lev == 0, ! ;
	    write('Non-global procedure definition:'), writeln(Decl), abort ),
	getAttr([procs:P,globalType:T,globalAddr:A], AttrsIn),
	processType(Decl,AttrsIn,Id,Type),
	(   varInEnv(Id,P)
	->  write('Invalid procedure name:'), writeln(Id), abort
	;   true ),
	(   varInEnv(Id,T), getEnv(Id,T,TypeOld), TypeOld \= Type
	->  write('Prototype and definition are inconsistent for procedure:'),
	    writeln(Id), abort
	;   true ),
	createEnv(Id,P,(Decl:::Body),NewP),
	createEnv(Id,T,Type,NewT),
	createEnv(Id,A,Id,NewA),
	updAttr([procs:NewP,globalType:NewT,globalAddr:NewA], AttrsIn, AttrsOut).

/* Process all procedures one by one, placing their
   definitions in the current attribute set */
collectProcs((Proc;Prog),AttrsIn,AttrsOut,Rest) :-
	Proc = (_ ::: _ ),!,
	collectProcsHelper(Proc,AttrsIn,AttrsAux),
	collectProcs(Prog,AttrsAux,AttrsOut,Rest).
collectProcs(Rest,Attrs,Attrs,Rest).

/* Phase 2: compilation of procedures
   -- converts the environment of procedures in
      of the current attribute set into a list
      of procedures, for easy compilation. */
compileProcs(CodeProcs,Attrs) :-
	getAttr([procs:[P]],Attrs),
	assoc_to_list(P,PL),
	compileProcList(PL,CodeProcs,Attrs).

/* Helper for Phase 2 */
compileProcList([],nocode,_Attrs) :- !.
compileProcList([Pname-(Proto:::Body)|Rest],Code,Attrs) :-
	compileProcedure(Pname,Proto,Body,CodeP,Attrs),
	compileProcList(Rest,CodeRest,Attrs),
	appendCode(CodeP,CodeRest,Code).

/* compute offsets for arguments wrt framePtr. The offsets are
   negative, and will be reversed by compileExpr when its argument
   is an identifier
   The first argument is allocated at framePtr+8 (to make room for
   the old framePtr and return address), so the offset for an empty
   list is -8. At every step, the allocated spaces varies by an amount
   equal to the size of the type of the current argument. */
argumentsEnv([],[],_Attrs,Empty,Empty,-8) :- !,expandEnv([],Empty).
argumentsEnv([A|As],[T|Ts],Attrs,Addrs,Types,K) :-
	argumentsEnv(As,Ts,Attrs,Ad,Ty,K1),
	createEnv(A,Ad,AdAux),
	putEnv(A,AdAux,K1,Addrs),
	createEnv(A,Ty,Taux),
	putEnv(A,Taux,T,Types),
	sizeOf(T,Attrs,S),
	K is K1 - S.

% compile one procedure
%  Arg1 (in) : name of procedure
%  Arg2 (in) : prototype of procedure
%  Arg3 (in) : body of procedure, of the form {S}
%  Arg4 (out): compiled code for procedure
%  Arg5 (in) : current attributes, for access to global
%              variables and possibly other procedures
compileProcedure(Pname,Proto,Body,Code,Attrs) :-
	getArgList(Proto,Args),
	processArgsTypes(Args,Attrs,Ids,TList),
	argumentsEnv(Ids,TList,Attrs,Addrs,Types,_),
	expandEnv([],EmptyEnv),
	updAttr([localAddr:Addrs,lTopAddr:0,localType:Types,
		 localStruct: EmptyEnv,level:1,
		 labelRequest:nolabel,returnLabel:RetLbl,
		 curProc:Pname,maxStack:0],Attrs,AttrsProc),
	% compile the body in an environment where the arguments
	% were added, appearing as regular variables
	compileStmtNS(Body,CodeBody,AttrsProc,AttrsBody),
	% get the stack space required by the procedure
	getAttr([maxStack:M],AttrsBody),
	% create optimized entry and exit code
	(   M == 0
	->  StackAllocation = nocode
	;   StackAllocation = ( stackPtr = stackPtr - M ) ),
	getAttr([labelRequest:L],AttrsBody), newreg(RetAddr),
	(   L == nolabel    ->  newlabel(RetLbl)  ;   RetLbl = L ),
	C1 = ( Pname :: stackPtr = stackPtr - 4 ;
	                [stackPtr] = framePtr  ;
	                framePtr = stackPtr
	     ),
	C2 = ( RetLbl :: stackPtr = framePtr + 8;
			 RetAddr  = [framePtr+4] ;
			 framePtr = [framePtr] ;
	                 goto RetAddr  ),
	% put it all together
	appendCode([C1,StackAllocation,CodeBody,C2],Code).

% Main compilation predicate
% Arg1 (in) : program to be compiled
% Arg2 (out): generated code
% Arg3 (out): output attributes -- addresses of global variables can
%	      be retrieved from here
compile(Prog,Code,AttrsOut) :-
	expandEnv([],EmptyEnv),
	% set up initial attributes
	updAttr(  [ globalAddr   : EmptyEnv, % maps global symbol -> address
		    gTopAddr     : 0,        % top address used in heap
		    globalType   : EmptyEnv, % maps global symbol -> symbol's type
		    globalStruct : EmptyEnv, % maps struct name -> struct definition
		    procs	 : EmptyEnv, % maps procedure name -> proc definition
		    localAddr    : EmptyEnv, % maps locally scoped symbols -> address
		    lTopAddr	 : 0,	     % top address used in stack
		    localType    : EmptyEnv, % maps local symbol -> symbol's type
		    localStruct	 : EmptyEnv, % maps local struct name -> struct definition
		    level        : 0,	     % scope level, 0 = global scope
		    labelRequest : nolabel,  % if different from nolabel, next chunk of code must start with this label
		    returnLabel  : nolabel,  % inside a procedure, the label for the exit code from a procedure
		    curProc	 : nolabel,  % name of the procedure being currently compiled: useful for type checking
		    maxStack     : 0         % stack space needed for the current scope,
		                             % as computed at the current point in the compilation process
		  ],
		  [], AttrsInit ),
	% compile the global declarations
	compileGlobalDecl(Prog,AttrsInit,AttrsGlobal,RestProg1),
	% collect all procedures
	collectProcs(RestProg1,AttrsGlobal,AttrsProcs,RestProg2),
	% compile the the main program
	compileStmt(RestProg2,CodeProg,AttrsProcs,AttrsOut),
	% compile collected procedures
	compileProcs(CodeProcs,AttrsOut),
	getAttr([labelRequest:L,maxStack:M],AttrsOut),
	% add entry and exit code to main program
	S is 10000-M,
	(   L == nolabel -> newlabel(LL) ; LL = L ),
	C = ( stackPtr = S ; framePtr = 10000 ),
	appendCode([C,CodeProg,(goto LL),CodeProcs,(LL::nop)],Code).
/*
:- Prog = (
	  int g#[int x]:::{
	    return x+x ;
	  } ;
	  int ($f#[])#[int x]:::{
	    return & g ;
	  };
	  {
	  int a ;
	  a = ($f#[])#[10] ;
	  } ;
	  ),
	resetnewreg, resetnewlabel,
	compile(Prog,Code,_A),
	writeTac(Code),
	tacToObj(Code,Obj),
	writeObj(Obj,0),
	empty_assoc(Empty),
	execObj(0,Obj,Empty,Empty,Env,Heap),
	writeln(Env), writeln(Heap).
*/
/*
:- Prog = (
          int a@[3], $ b, $ $ c, $ $ $ d ;
	  d = & c ;
	  c = & b ;
	  b = a ;
	  $ ($ $ d + 1) = 10 ;
	  a@[1] = a@[1]+1 ;
	  ),
	resetnewreg, resetnewlabel,
	compile(Prog,Code,_A),
	writeTac(Code),
	tacToObj(Code,Obj),
	writeObj(Obj,0),
	empty_assoc(Empty),
	execObj(0,Obj,Empty,Empty,Env,Heap),
	writeln(Env), writeln(Heap).
*/
/*
:- Prog = (
          int a, b ;
	  int $ f#[int x] ::: {
	    return x ? &a : &b ;
	  } ;
	  {
	   $ f#[3] = 10 ;
	  };
	  ),
	resetnewreg, resetnewlabel,
	compile(Prog,Code,_A),
	writeTac(Code),
	tacToObj(Code,Obj),
	writeObj(Obj,0),
	empty_assoc(Empty),
	execObj(0,Obj,Empty,Empty,Env,Heap),
	writeln(Env), writeln(Heap).

*/
:- Prog = (
	  int f#[int x] ::: {
            str~~~{int $d , w@[10], c ; };
	    str~~~(s@[20]) ;
	    (s@[10]~~w)@[5] = x ;
	    return 1 ;
	  } ;
	  {
	  int x ;
	  x = f#[3] ;
	  };
	  ),
	resetnewreg, resetnewlabel,
	compile(Prog,Code,_A),
	writeTac(Code),
	tacToObj(Code,Obj),
	writeObj(Obj,0),
	empty_assoc(Empty),
	execObj(0,Obj,Empty,Empty,Env,Heap),
	writeln(Env), writeln(Heap).

/*

:- Prog = (
	  int a, b, c;
	  int p#[int x, int y] ::: {
	     str~~~{int $d , w@[10], c ; };
	     int z@[10] ;
	     str~~~(s@[20]) ;
	     (s@[10]~~w)@[5] = x+y ;
	     s@[10]~~c = x ;
	     s@[10]~~d = &a ;
	     $(s@[10]~~d) = 1 ;
	     a=((s@[10])~~w)@[5] ;
	     return s@[10]~~c ;
	  } ;
	  {
	   int b ;
	  b = p#[2,3] ;
	  c = b ;
	  };
	  ),
	resetnewreg, resetnewlabel,
	compile(Prog,Code,_A),
	writeTac(Code),
	tacToObj(Code,Obj),
	writeObj(Obj,0),
	empty_assoc(Empty),
	execObj(0,Obj,Empty,Empty,Env,Heap),
	writeln(Env), writeln(Heap).
*/
/*
:- Prog = (
	  int a, b, c;
	  int e@[5] ;
	  str~~~{int x; int $y;};
	  str~~~f ;
	  int p#[int $x, str~~~ ($y)] ::: {
	     c = 1 ;
	     return x@[5]+(($y)~~x) ;
	  } ;
	  e@[2] = 10 ;
	  f~~x = 20 ;
	  b = p#[&a,&f] ;
	  c = b ;
	  ),
	resetnewreg, resetnewlabel,
	compile(Prog,Code,_A),
	writeTac(Code),
	tacToObj(Code,Obj),
	writeObj(Obj,0),
	empty_assoc(Empty),
	execObj(0,Obj,Empty,Empty,Env,Heap),
	writeln(Env), writeln(Heap).
*/
/*


:- resetnewreg, resetnewlabel.
*/
/*
:- Prog = (
          int a ;
	  int gcd#[int x, int y] ::: {
	     return ((x==y) ? x : ( (x < y) ? (gcd#[x,y-x]) : (gcd#[x-y,y]) ) );
	  } ;
	  a = gcd#[144,60] ;
	  ),
	compile(Prog,Code,ST),
	writeTac(Code),
	tacToObj(Code,Obj),
	writeObj(Obj,0),
	empty_assoc(Empty),
	execObj(0,Obj,Empty,Empty,Env,Heap),
	writeln(ST),
	writeln(Env), writeln(Heap).
*/





