
/*
 * High-level language: the statements of 03.pl and 'switch'
 * Low-level language: three adress code with jumps as in 02.pl and 03.pl (no change)
 *
 * As before we have:
 *   - syntax checker and evaluator for expressions
 *   - syntax checker and evaluator for TAC
 *   - compiler from expressions to TAC
 *
 * The acid test of the compiler is to verify that the result
 * of evaluating an expression is identical to the result
 * obtained by first compiling the expression into TAC, and then
 * evaluating the compiled TAC code.
 */

/*
 *  Arithmetic expressions as in 02.pl, 03.pl
 */

% Operator declarations
:- op(800,yfx,and).
:- op(810,yfx,or).

% Syntax checker
isExpr(X ? Y : Z) :- isExpr(X), isExpr(Y), isExpr(Z).
isExpr(X) :-
        X =.. [F,A,B],
        member(F,[ +, -, *, /, mod, and, or, /\, \/, <<, >> , xor,
                  < , > , =<, >= , == , \= ]),!,
        isExpr(A), isExpr(B).
isExpr(X) :-
        X =.. [F,A],
        member(F,[+,-,\]), !,
        isExpr(A).
isExpr(X) :-
        integer(X),! ; atom(X), \+ atom_prefix(X,v_).


% Syntax checker for statements: 03.pl augmented with rules for 'switch'

:- op(1099,yf,;).
:- op(960,fx,if).
:- op(959,xfx,then).
:- op(958,xfx,else).
:- op(960,fx,while).
:- op(959,xfx,do).
:- op(960,fx,switch).
:- op(959,xfx,of).
:- op(970,xfx,::).

isStmtNS((X=Expr)) :- !,atom(X),\+ atom_prefix(X,v_), isExpr(Expr).
isStmtNS((while B do { S })) :-  !, isExpr(B), isStmt(S).
isStmtNS((if B then { S1 } else { S2 })) :- !, isExpr(B), isStmt(S1), isStmt(S2).
isStmtNS((if B then { S } )) :- !, isExpr(B), isStmt(S).
isStmtNS({S}) :- !,isStmt(S).
isStmtNS((S1;S2)) :- !, isStmtNS(S1), isStmt(S2).
isStmtNS((switch Expr of { CL })) :- !, isExpr(Expr), isCaseList(CL,0).
isStmt((S;)) :- !, isStmtNS(S).
isStmt((S1;S2)) :- isStmtNS(S1), isStmt(S2).

% helper predicate for body of switch
isCaseList((L::{S};CL),N) :- !, integer(L), L >= N, isStmt(S), N1 is N +1, isCaseList(CL,N1).
isCaseList((default::{S};),_) :- isStmt(S).

% quirks of the 'switch' statement
%  - no 'case' keyword
%  - code for each arm must be enclosed in braces
%  - closing brace of each arm must be followed by semicolon
%  - labels must be positive and appear in increasing order
%  - labels are followed by '::' instead of ':'
%  - there must be a 'default' label, whose arm must appear last
%  - there is no 'break' statement, but each arm executes as if it were
%      terminated by a 'break'

% Test statement syntax checker

:- Code = (
	    x = 1 ;
            y = 0 ;
            switch x of {
            1::{ while ( x > 0 ) do {
                    y = y + x ;
                    x = x - 1 ;
                 } ;
               };
            default::
	       { if ( y > 50 ) then
                  { x = 1 ; }
                 else {
                    if ( y > 100) then {
                      x = 2 ;
                    } ;
                 } ;
               };
            } ;
          ), isStmt(Code),
          writeln('==========================='),
          writeln('Tested code:'), writeln(Code), writeln('Passed'),nl.


% Operational semantics: interpreter for HL language
%  all rules of 03.pl brought in unchanged, augmented with rules for 'switch'
evalExpr(E,E,_) :- integer(E),!.
evalExpr(E,Val,Env) :- atom(E),!,get_assoc(E,Env,Val).
evalExpr(X+Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),Val is Vx+Vy.
evalExpr(X-Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),Val is Vx-Vy.
evalExpr(X*Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),Val is Vx*Vy.
evalExpr(X/Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),Val is Vx//Vy.
evalExpr(X mod Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),Val is Vx mod Vy.
evalExpr(X /\ Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),Val is Vx /\ Vy.
evalExpr(X \/ Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),Val is Vx \/ Vy.
evalExpr(X and Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),Val is (Vx /\ Vy)/\1.
evalExpr(X or Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),Val is (Vx \/ Vy)/\1.
evalExpr(X xor Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),Val is Vx xor Vy.
evalExpr(X << Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),Val is Vx << Vy.
evalExpr(X >> Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),Val is Vx >> Vy.
evalExpr(+ X,Val,Env) :- !,evalExpr(X,Vx,Env), Val is Vx .
evalExpr(- X,Val,Env) :- !,evalExpr(X,Vx,Env), Val is - Vx .
evalExpr(\ X,Val,Env) :- !,evalExpr(X,Vx,Env), Val is \ Vx .
evalExpr(X < Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),
        (   Vx < Vy -> Val = 1 ; Val = 0 ).
evalExpr(X > Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),
        (   Vx > Vy -> Val = 1 ; Val = 0 ).
evalExpr(X =< Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),
        (   Vx =< Vy -> Val = 1 ; Val = 0 ).
evalExpr(X >= Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),
        (   Vx >= Vy -> Val = 1 ; Val = 0 ).
evalExpr(X == Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),
        (   Vx = Vy -> Val = 1 ; Val = 0 ).
evalExpr(X \= Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),
        (   Vx \= Vy -> Val = 1 ; Val = 0 ).
evalExpr(\+ X,Val,Env) :- !,evalExpr(X,Vx,Env), Vx \= 0 -> Val is 0 ; Val is 1 .
evalExpr(X ? Y : Z, Val, Env) :- !,
        evalExpr(X,Vx,Env), (Vx \= 0 -> evalExpr(Y,Val,Env) ; evalExpr(Z,Val,Env)).

execHL((X = E),EnvIn,EnvOut) :- !,
        evalExpr(E,Val,EnvIn), put_assoc(X,EnvIn,Val,EnvOut).
execHL((while B do { S }),EnvIn,EnvOut) :-  !,
        execHL((if B then { S ; while B do { S } ; }),EnvIn,EnvOut).
execHL((if B then { S1 } else { S2 }),EnvIn,EnvOut) :- !,
        evalExpr(B,Val,EnvIn),
        (   Val \= 0
        ->  execHL(S1,EnvIn,EnvOut)
        ;   execHL(S2,EnvIn,EnvOut) ).
execHL((if B then { S } ),EnvIn,EnvOut) :- !,
        evalExpr(B,Val,EnvIn),
        (   Val \= 0
        ->  execHL(S,EnvIn,EnvOut)
        ;   EnvIn = EnvOut ).
% switch is handled as syntactic sugar -- translated into an 'if'
execHL((switch B of {L::{S};C}),EnvIn,EnvOut) :- !,
        execHL((if B == L then { S } else { switch B of { C }}),EnvIn,EnvOut).
execHL((switch _ of {default::{S};}),EnvIn,EnvOut) :- !,
        execHL(S,EnvIn,EnvOut).
execHL({S},EnvIn,EnvOut) :- !,execHL(S,EnvIn,EnvOut).
execHL((S1;S2),EnvIn,EnvOut) :- !, execHL(S1,EnvIn,EnvAux), execHL(S2,EnvAux,EnvOut).
execHL((S;),EnvIn,EnvOut) :- !,execHL(S,EnvIn,EnvOut).

% Test interpreter
:- Code = (
                       x = 3 ;
                       y = 200 ;
                       switch x of {
                       1::{ while ( x > 0 ) do {
                              y = y + x ;
                              x = x - 1 ;
                            } ;
                          };
                       default::{ if ( y > 50 ) then
                              { x = 1 ; }
                            else {
                              if ( y > 100) then {
                                 x = 2 ;
                              } ;
                            } ;
                          };
                       } ;
          ), isStmt(Code),
          writeln('==========================='),
          writeln('Code tested:'), writeln(Code),
          empty_assoc(Empty),
          execHL(Code,Empty,EO),
          write('Output values x = '), get_assoc(x,EO,Vx), writeln(Vx),nl.

/*
 *  Three-address intermediate code with jumps - same as 02.pl and 03.pl
 */

:- op(950,fx,goto).
:- op(950,xfx,goto).
:- op(970,xfy,::). % We allow multiple labels on a instruction
:- op(969,xf,::).


isTaiNL(nop) :- !.  /* No operation */
isTaiNL(X = Y) :-  % is argument a three-address-instruction with no label?
        atom(X),
        (   atom(Y) ; integer(Y) ;
           (   Y =.. [F,A,B],
               (   atom(A),! ; integer(A)   ),
               (   atom(B),! ; integer(B)   ),
               member(F,[+,-,*,/,mod,/\,\/,xor,<<,>>]) ) ),!.
isTaiNL(X= \ Y) :- atom(X), (atom(Y) ; integer(Y)),!.
isTaiNL(goto X) :-
        ( atom(X) ; integer(X); ( X=..[F,A,B], (F = + ; F = -), atom(A), (atom(B);integer(B)) ) ),!.
isTaiNL(if E goto Y) :-
        E =.. [F,X,0], member(F,[==,\=,<,>,=<,>=]),
        (atom(X);integer(X)),
        (  atom(Y);integer(Y);
          ( Y=..[F1,A,B], (F1 = + ; F1 = -), atom(A), (atom(B);integer(B)) ) ),!.

isTai((L::I)) :- !,atom(L), ( I =.. [(::)|_] -> isTai(I) ; isTaiNL(I)).
isTai((L::)) :- !,atom(L).
isTai(I) :- isTaiNL(I).

isTac(X) :- isTai(X),!.
isTac(X;) :- isTai(X),!.
isTac((X;Y)) :- isTac(X), isTac(Y).

alignLabel(X,Y) :-
        atomic_concat(X,'          ',Z),
        atom_chars(Z,L), append(L1,_,L), length(L1,10),
        atomic_list_concat(L1,Y).

writeTac((X;Y)) :- !, writeTac(X), writeTac(Y).
writeTac((X::Y)) :- !, alignLabel(X,X1),
                    (   Y =.. [(::)|_]
                     -> write(X1), writeln((::)), writeTac(Y)
                     ;  writeln((X1::Y)) ).
writeTac((X::)) :- !, alignLabel(X,X1), write(X1), writeln((::)).
writeTac(X) :- write('            '),writeln(X).

taiNLtoObj(nop,P,P,nop) :- !.
taiNLtoObj(X=Y,P,P,X=Y) :- !.
taiNLtoObj(Iin,P,[X:Y|P],Iout) :-
        (   Iin = (goto E), Iout = (goto D)
        ;   Iin = (if Expr goto E), Iout = (if Expr goto D) ),
        (   E =.. [F,X,Z], D =.. [F,Y,Z]
        ;   atom(E), E = X, D = Y ) ,!.
taiNLtoObj(_I,_Pin,_Pout,_T).
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
firstpass((C1;C2),IPin,IPout,Lin,Lout,Pin,Pout,Tin,Tout) :- !,
        firstpass(C1,IPin,IPaux,Lin,Laux,Pin,Paux,Tin,Taux),
        firstpass(C2,IPaux,IPout,Laux,Lout,Paux,Pout,Taux,Tout).
firstpass((I;),IPin,IPout,Lin,Lout,Pin,Pout,Tin,Tout) :- !,
        taiToObj(I,IPin,Lin,Lout,Pin,Pout,T),
        put_assoc(IPin,Tin,T,Tout),
        IPout is IPin + 1.
firstpass(I,IPin,IPout,Lin,Lout,Pin,Pout,Tin,Tout) :-
        taiToObj(I,IPin,Lin,Lout,Pin,Pout,T),
        ( T = none
          -> IPout = IPin, Tout = Tin
          ; put_assoc(IPin,Tin,T,Tout), IPout is IPin + 1 ).
secondpass([],_).
secondpass([Lbl:P|T],L) :-
        get_assoc(Lbl,L,P) -> secondpass(T,L)
        ; write('Undefined label:'), writeln(Lbl), abort.

tacToObj(SourceCode,ObjectCode) :- % main translation predicate
        empty_assoc(Empty),
        firstpass(SourceCode,0,_,Empty,Labels,[],Placeholders,Empty,ObjectCode),
        secondpass(Placeholders,Labels).

alignIP(X,Y) :-
        atomic_concat('   ',X,Z),
        atom_chars(Z,L), append(_,L1,L), length(L1,3),!,
        atomic_list_concat(L1,Y).
writeObjectcode(Obj,IP) :- max_assoc(Obj,K,_), IP > K, !.
writeObjectcode(Obj,IP) :-
        get_assoc(IP,Obj,I), alignIP(IP,X),write(X), write(' :: '), writeln(I),
        IPnext is IP + 1, writeObjectcode(Obj,IPnext).

execTai(E,nop,E) :- !.
execTai(Env0,(X=Y),Env1) :-
        evalExpr(Y,Val,Env0),
        put_assoc(X,Env0,Val,Env1).
execObj(Env,IP,Code,Env) :-
        max_assoc(Code,K,_), IP > K, !.
execObj(EnvIn,IP,Code,EnvOut) :-
        get_assoc(IP,Code,Instr),
        (   Instr = (goto L), !, evalExpr(L,IPnext,EnvIn), EnvAux = EnvIn
        ;   Instr = (if E goto L), evalExpr(E,Val,EnvIn), EnvAux = EnvIn,
                  ( Val = 1
                    -> evalExpr(L,IPnext,EnvIn)
                    ;  IPnext is IP + 1 )
        ;   execTai(EnvIn,Instr,EnvAux), IPnext is IP+1 ),
        execObj(EnvAux,IPnext,Code,EnvOut).


/********************************************
 * Compiler from HL language to TAC
 ********************************************/

% all compiler rules from 03.pl imported here, and augmented with rules for 'switch'

:- dynamic auxvar/1.

% generate new variable names - same as 02.pl
newvar(X) :- atom(X),!.
newvar(X) :-
        retract(auxvar(Y))
        ->  Y1 is Y+1, assert(auxvar(Y1)), atomic_concat('v_',Y1,X)
        ;   assert(auxvar(0)), X = 'v_0' .

resetnewvar :-
        (   retractall(auxvar(_)) -> true ; true ),
        assert(auxvar(0)).

list_prefix(_,0,[]):-!.
list_prefix([H|T],N,[H|L]) :- N1 is N-1, list_prefix(T,N1,L).

% generate new label names - same as 02.pl
newlabel(X) :-
        retract(auxlbl(Y))
        ->  Y1 is Y+1, assert(auxlbl(Y1)), atomic_concat('l_',Y1,X)
        ;   assert(auxlbl(0)), X = 'l_0' .

resetnewlabel :-
        (   retractall(auxlbl(_)) -> true ; true ),
        assert(auxlbl(0)).

% compilation of expressions is the same as 02.pl
compile(E,(X=E),X) :- (integer(E);atom(E)),!,newvar(X).
compile(E,Code,R) :-
        E =.. [Op,X,Y], member(Op,[+,-,*,/,mod,/\,\/,xor,<<,>>]), !,
        compile(X,Cx,Rx), compile(Y,Cy,Ry),
        newvar(R), CE =.. [Op,Rx,Ry],
        Code = ( Cx ; Cy ; R = CE ) .
compile(+ X,Code,R) :- !,compile(0+X,Code,R).
compile(- X,Code,R) :- !,compile(0-X,Code,R).
compile(\ X,Code,R) :- !,compile((-1) xor X,Code,R).
compile(Cond, Code, R) :-
        Cond =.. [Op,X,Y], member(Op,[<,>,=<,>=,==,\=]), !,
        newvar(R), newlabel(Skip), newlabel(Lout),
        compile(X-Y,C1,Q), Cond1 =.. [Op,Q,0],
        C2 = ( if Cond1 goto Skip ; R = 0 ; goto Lout ; Skip::R = 1 ; Lout ::  ),
        Code = (C1 ; C2).
compile((X ? Y : Z), Code, R) :- !,
        newvar(R), newlabel(Skip), newlabel(Lout),
        compile(X,Cx,Qx),
        compile(Y,Cy,Qy),
        compile(Z,Cz,Qz),
        Code = (   Cx                   ;
                   if Qx == 0 goto Skip ;
                   Cy                   ;
                   R = Qy               ;
                   goto Lout            ;
            Skip ::                     ;
                   Cz                   ;
                   R = Qz               ;
            Lout ::                      ).

%%%%%
%  Compilation of statements -- new in 03.pl
%%%%%
compile((X=Expr),Code,X) :- !,compile(Expr,Code,X).
compile((S1;S2),(C1;C2),R) :- !,
        compile(S1,C1,_), compile(S2,C2,R).
compile((S;),Code,R) :- compile(S,Code,R).
compile(({S}),Code,R) :- compile(S,Code,R).
compile((while B do { S }),Code,R) :-  !,
        newlabel(Lout), newlabel(Ltop),
        compile(B,CodeB,RB),
        C1 = (if RB == 0 goto Lout),
        compile(S,CodeS,R),
        C2 = (goto Ltop),
        Code = (Ltop::;CodeB;C1;CodeS;C2;Lout::).
compile((if B then S1 else S2),Code,R) :- !,
        compile(B,CodeB,R),
        newlabel(Lfalse), newlabel(Lout),
        C1 = (if R == 0 goto Lfalse),
        compile(S1,CodeS1,_),
        C2 = (goto Lout),
        compile(S2,CodeS2,_),
        Code = (CodeB;C1;CodeS1;C2;Lfalse::;CodeS2;Lout::).
compile((if B then S),Code,R) :- !,
        newlabel(Lout),
        compile(B,CodeB,R),
        C = (if (R == 0) goto Lout),
        compile(S,CodeS,_),
        Code = (CodeB;C;CodeS;Lout::).
compile((switch B of { CL }),Code,R) :- !,
	newlabel(Lstart), newlabel(Lout),
        compile(B,CodeB,R),
        compileCaseList(CL,CodeCL,Lstart,Lout,Ldefault,SegmentLength,MaxN),
        C1 = (if R > MaxN goto Ldefault ),
        newvar(R1), C = (C1;R1 = R * SegmentLength ; goto Lstart+R1),
        Code = (CodeB ; C ; Lstart:: ; CodeCL ; Lout::).

% Compilation of 'switch' body relies on compiling all
% the arms into TAC code of the same length. Then, the indexed
% jump 'goto Lstart+Switch_arg*Arm_length' (one such jump appears in the rule above)
% can be used to reach the corresponding arm.
% This compilation process uses 4 helper predicates.

% First, all arms are placed in a dictionary indexed by labels
% In the process, labels to the first arm, default arm, and the
% label right after the 'switch' statement are singled out
cclCodeDict((L::{S};CL),CodeDictIn,CodeDictOut,Lstart,Ldefault,Lout) :- !,
        newlabel(L0), atomic_concat(L0,'_',L1),
        term_to_atom(L,L3), atomic_concat(L1,L3,Lin),
        compile(S,CS,_),
        C = (goto Lout),
        put_assoc(L,CodeDictIn,(Lin::;CS;C),CodeDictAux),
        cclCodeDict(CL,CodeDictAux,CodeDictOut,Lstart,Ldefault,Lout).
cclCodeDict((default::{S};),CodeDictIn,CodeDictOut,_Lstart,Ldefault,Lout) :- !,
        newlabel(L0), atomic_concat(L0,'_d',Ldefault),
        compile(S,CS,_),
        put_assoc(default,CodeDictIn,(Ldefault::;CS;goto Lout),CodeDictOut).

% Number of instructions in a piece of TAC code - useful for padding
% of switch cases
tacSize((_::),0).
tacSize(X,1) :- isTai(X),!.
tacSize((X;),1) :- isTai(X),!.
tacSize((X;Y),N) :- tacSize(X,Nx), tacSize(Y,Ny), N is Nx+Ny.

% Second helper predicate computes the sizes of all the arms,
% so that they can be subsequently padded to have equal length
% The lengths of each arm are placed in a dictionary. The maximum
% arm size, and the largest label (except default) are singled out
cclSizes([],S,S,0,0) :- !.
cclSizes([K-C|L],SizeDictIn,SizeDictOut,MaxSize,MaxLabel) :-
        tacSize(C,LC), put_assoc(K,SizeDictIn,LC,SizeDictAux),
        cclSizes(L,SizeDictAux,SizeDictOut,M,ML),
        (   K == default
        ->  MaxLabel = ML, MaxSize = 0
        ;   MaxLabel is max(K,ML), MaxSize is max(LC,M) ).

padding(1,nop) :- !.
padding(N,(nop;Code)) :- N1 is N-1, padding(N1,Code).

% Third helper predicate: pads all the 'switch' arms so
% that they have equal length
cclPad([default-Code],CodeDictIn,CodeDictOut,_,_) :- !,
        put_assoc(default,CodeDictIn,Code,CodeDictOut).
cclPad([K-C|L],CodeDictIn,CodeDictOut,SizeDict,MaxSize) :-
        get_assoc(K,SizeDict,Sz), P is MaxSize-Sz,
        (   P > 0
        ->  padding(P,Nops), Code = (C;Nops)
        ;   Code = C ),
        put_assoc(K,CodeDictIn,Code,CodeDictAux),
        cclPad(L,CodeDictAux,CodeDictOut,SizeDict,MaxSize).

% Fourth helper predicate lays out all the arms into a contiguous
% stretch of code. Code for the missing labels is also generated here,
% in the form of a jump to the default label.
cclLayout(CodeDict,_Ldefault,_SegmentSize,Lbl,MaxLbl,CodeIn,(CodeIn;Code)) :-
        Lbl > MaxLbl, !,
        get_assoc(default,CodeDict,Code).
cclLayout(CodeDict,Ldefault,SegmentSize,Lbl,MaxLbl,CodeIn,CodeOut) :-
        (   get_assoc(Lbl,CodeDict,Code)
        ->  true
        ;   newlabel(NL), atomic_concat(NL,'_',L1),
            term_to_atom(Lbl,L3), atomic_concat(L1,L3,LL),
            Sz is SegmentSize - 1,
            (        Sz > 0
            ->        padding(Sz,Nops), Code = (LL::goto Ldefault ; Nops)
            ;        Code = (LL::goto Ldefault) )
        ),
        L is Lbl+1, cclLayout(CodeDict,Ldefault,SegmentSize,L,MaxLbl,(CodeIn;Code),CodeOut).

% the 'switch' body compilation predicate sequences all the
% four helper predicates
compileCaseList(CL,Code,Lstart,Lend,Ldefault,MaxN,MaxLbl) :-
        empty_assoc(Empty), cclCodeDict(CL,Empty,CDNP,Lstart,Ldefault,Lend),
        assoc_to_list(CDNP,CodeList),
        cclSizes(CodeList,Empty,SizeDict,MaxN,MaxLbl),
        cclPad(CodeList,Empty,CodeDict,SizeDict,MaxN),
        (   get_assoc(0,CodeDict,CodeIn)
        ->  true
        ;   newlabel(NL), atomic_concat(NL,'_0',LL),
            Sz is MaxN - 1,
            (        Sz > 0
            ->        padding(Sz,Nops), CodeIn = (LL::goto Ldefault;Nops)
            ;        CodeIn = (LL::goto Ldefault) )
        ),
        cclLayout(CodeDict,Ldefault,MaxN,1,MaxLbl,CodeIn,Code).

% Main compiler predicate, adds a labeled 'nop' at end of code
% if request for label is made by 'compile'
compileHL(Stmt,Code) :-
   compile(Stmt,Code,_Res).

% Test compiler
:- resetnewvar, resetnewlabel.
:- Code = (
          a = 1 ;
          switch a of {
          0:: { x = 1 ; z = x+1 ;} ;
          2:: { x = 2 ; x = x - 1 ; z = x << 3 ; } ;
          default:: {
                      x = 10 ;
                      y = 5 ;
                      z = 0 ;
                      while ( y > 0 ) do {
                          z = z + x ;
                          y = y - 1 ;
                      } ;
                    } ;
          };
          ), compileHL(Code,Tac),
	  writeln('================================================'),
	  writeln('Code tested:'),
	  writeln(Code),
	  writeln('TAC:'),
          writeTac(Tac),
          empty_assoc(Empty),
          execHL(Code,Empty,Rexec),
          tacToObj(Tac,Obj),
	  writeln('Object code:'),
	  writeObjectcode(Obj,0),
          execObj(Empty,0,Obj,Robj),
          write('Interpretation result: z = '), get_assoc(z,Rexec,Ve),writeln(Ve),
          write('Object code result: z = '), get_assoc(z,Robj,Vo),writeln(Vo),nl.

