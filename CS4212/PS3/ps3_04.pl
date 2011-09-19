% Benjamin Tan Wei Hao
% U077129N

% My additions to the code are surrounded by "%%%%%%%%%%%%%%%%%%"

% Problem Set 3, Exercise 4.

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

% operator declarations that make statements possible
:- op(1099,yf,;).
:- op(960,fx,if).
:- op(959,xfx,then).
:- op(958,xfx,else).
:- op(960,fx,while).
:- op(959,xfx,do).

% Syntax checker for statements: assignment, if, while
% High level language has a syntax quirk:
%   every statement MUST be ended with a semicolon
%   the semicolon MUST appear even after closing braces
%   however, we don't put a semicolon before an else

% checks for statements with no semicolons
isStmtNS((X=Expr)) :- !,atom(X),\+ atom_prefix(X,v_), isExpr(Expr).
isStmtNS((while B do { S })) :-  !, isExpr(B), isStmt(S).
isStmtNS((if B then { S1 } else { S2 })) :- !, isExpr(B), isStmt(S1), isStmt(S2).
isStmtNS((if B then { S } )) :- !, isExpr(B), isStmt(S).
%%%%%%%%%%%%%%%%%%
isStmtNS((break)) :- !.
isStmtNS((continue)) :- !.
%%%%%%%%%%%%%%%%%%
isStmtNS({S}) :- !,isStmt(S).
% statements with semicolons
%%%%%%%%%%%%%%%%%%
isStmt((break;)) :- !.
isStmt((continue;)) :- !.
%%%%%%%%%%%%%%%%%%
isStmt((S;)) :- !, isStmtNS(S).
isStmt((S1;S2)) :- isStmtNS(S1), isStmt(S2).


% Test statement syntax checker

% :- Code = (
%                        x = 10 ;
%                        y = 0 ;
%                        while ( x > 0 ) do {
%                           y = y + x ;
%                           x = x - 1 ;
%                           continue ;
%                           if ( x < 0 ) then {
%                             y = x + 1 ;
%                             break ;
%                           } ;
%                        } ;
%                        if ( y > 50 ) then
%                          { x = 1 ; }
%                        else {
%                          if ( y > 100) then {
%                             x = 2 ;
%                             continue ;
%                             break ;
%                          } ;
%                        } ;
%           ), isStmt(Code),
%           writeln('==========================='),
%           writeln('Tested code:'), writeln(Code), writeln('Passed'),nl.

/*
 * Operational semantics: interpreter for HL language
 */
% expression evaluator imported from 02.pl
evalExpr(E,E,_) :- integer(E),!.
evalExpr(E,Val,Env) :- atom(E),!,get_assoc(E,Env,Val).
evalExpr(X+Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx+Vy.
evalExpr(X-Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx-Vy.
evalExpr(X*Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx*Vy.
evalExpr(X/Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx//Vy.
evalExpr(X mod Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx mod Vy.
evalExpr(X /\ Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx /\ Vy.
evalExpr(X \/ Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx \/ Vy.
evalExpr(X and Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is abs(sign(Vx)) /\ abs(sign(Vy)).
evalExpr(X or Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is abs(sign(Vx)) \/ abs(sign(Vy)).
evalExpr(X xor Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx xor Vy.
evalExpr(X << Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx << Vy.
evalExpr(X >> Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx >> Vy.
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

% execution of statements
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

execHL({S},EnvIn,EnvOut) :- !,execHL(S,EnvIn,EnvOut).
execHL((S1;S2),EnvIn,EnvOut) :- !, execHL(S1,EnvIn,EnvAux), execHL(S2,EnvAux,EnvOut).
execHL((S;),EnvIn,EnvOut) :- !,execHL(S,EnvIn,EnvOut).


% Test interpreter - Do not bother with intepreting 'break' and 'continue' statements.

:- op(950,fx,goto).
:- op(950,xfx,goto).
:- op(970,xfy,::). % We allow multiple labels on a instruction
:- op(969,xf,::).

/****************************************
  3-address code, same as 02.pl
*****************************************/

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

execTAC(EnvIn,Code,EnvOut) :- tacToObj(Code,Obj), execObj(EnvIn,0,Obj,EnvOut).

/********************************************
 * Compiler from HL language to TAC
 ********************************************/

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

compile((break),Code,_, Lout, _) :-  !, Code = (goto Lout).
compile((continue),Code,_, _, Ltop) :-  !, Code = (goto Ltop).
compile(E,(X=E),X,_,_) :- (integer(E);atom(E)),!,newvar(X).
compile(E,Code,R,BreakLabel,ContLabel) :-
        E =.. [Op,X,Y], member(Op,[+,-,*,/,mod,/\,\/,xor,<<,>>]), !,
        compile(X,Cx,Rx,BreakLabel,ContLabel), compile(Y,Cy,Ry,BreakLabel,ContLabel),
        newvar(R), CE =.. [Op,Rx,Ry],
        Code = ( Cx ; Cy ; R = CE ) .
compile(+ X,Code,R,BreakLabel,ContLabel) :- !,compile(0+X,Code,R,BreakLabel,ContLabel).
compile(- X,Code,R,BreakLabel,ContLabel) :- !,compile(0-X,Code,R,BreakLabel,ContLabel).
compile(\ X,Code,R,BreakLabel,ContLabel) :- !,compile((-1) xor X,Code,R,BreakLabel,ContLabel).
compile(Cond, Code, R,BreakLabel,ContLabel) :-
        Cond =.. [Op,X,Y], member(Op,[<,>,=<,>=,==,\=]), !,
        newvar(R), newlabel(Skip), newlabel(Lout),
        compile(X-Y,C1,Q,BreakLabel,ContLabel), Cond1 =.. [Op,Q,0],
        C2 = ( if Cond1 goto Skip ; R = 0 ; goto Lout ; Skip::R = 1 ; Lout ::  ),
        Code = (C1 ; C2).
compile((X ? Y : Z), Code, R,BreakLabel,ContLabel) :- !,
        newvar(R), newlabel(Skip), newlabel(Lout),
        compile(X,Cx,Qx,BreakLabel,ContLabel),
        compile(Y,Cy,Qy,BreakLabel,ContLabel),
        compile(Z,Cz,Qz,BreakLabel,ContLabel),
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

    
compile((X=Expr),Code,X,BreakLabel,ContLabel) :- !,compile(Expr,Code,X,BreakLabel,ContLabel).
compile((S1;S2),(C1;C2),R,BreakLabel,ContLabel) :- !,
        compile(S1,C1,_,BreakLabel,ContLabel), compile(S2,C2,R,BreakLabel,ContLabel).
compile((S;),Code,R,BreakLabel,ContLabel) :- compile(S,Code,R,BreakLabel,ContLabel).
compile(({S}),Code,R,BreakLabel,ContLabel) :- compile(S,Code,R,BreakLabel,ContLabel).


%%%%%%%%%%%%%%%%%%
% 'while' augmented with 2 more fields: Break Label and Continue Label
compile((while B do { S }),Code,R,_,_) :-  !,
        newlabel(Lout), newlabel(Ltop),
        compile(B,CodeB,RB,_,_),
        C1 = (if RB == 0 goto Lout),
        compile(S,CodeS,R,Lout,Ltop),    % Pass the 'break' and 'continue' labels
        C2 = (goto Ltop),
        Code = (Ltop::;CodeB;C1;CodeS;C2;Lout::).
%%%%%%%%%%%%%%%%%%

compile((if B then S1 else S2),Code,R,_,_) :- !,
        compile(B,CodeB,R,_,_),
        newlabel(Lfalse), newlabel(Lout),
        C1 = (if R == 0 goto Lfalse),
        compile(S1,CodeS1,_,_,_),
        C2 = (goto Lout),
        compile(S2,CodeS2,_,_,_),
        Code = (CodeB;C1;CodeS1;C2;Lfalse::;CodeS2;Lout::).

compile((if B then S),Code,R, _, _) :- !,
        newlabel(Lout),
        compile(B,CodeB,R,_,_),
        C = (if (R == 0) goto Lout),
        compile(S,CodeS,_,_,_),
        Code = (CodeB;C;CodeS;Lout::).


% Main compiler predicate, ignores the result argument
compileHL(Stmt,Code) :- compile(Stmt,Code,_Res,_,_).


% Test compiler
:- resetnewvar, resetnewlabel.

:- Code = ( % Showcasing 'break' and 'continue'
            x = 10 ;
            a = 0 ;
            while ( x > 0 ) do {
              y = 10 ;
              break ;
              while ( y > 0 ) do {
                z = 10 ;
                while ( z > 0 ) do {
                   a = a + 1 ;
                   z = z - 1 ;
                   continue ;
                } ;
                y = y - 1 ;
                break ;
              } ;
              x = x - 1 ;
            } ;
          ), isStmt(Code),
          writeln('======================================='),
          writeln('Code tested:'), writeln(Code),
          compileHL(Code,Tac),
          writeln('Object code'),
          writeTac(Tac).
