/*
  Benjamin Tan Wei Hao (U077129N)
  Problem Set 3 , Execise 3

  My additions are enclosed in '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
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
isStmtNS({S}) :- !,isStmt(S).
% statements with semicolons
isStmt((S;)) :- !, isStmtNS(S).
isStmt((S1;S2)) :- isStmtNS(S1), isStmt(S2).

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
        get_assoc(IP,Obj,I), alignIP(IP,X),write(X), write(' :: '), writeln(I), !.

execTai(E,nop,E) :- !.
execTai(Env0,(X=Y),Env1) :-
        evalExpr(Y,Val,Env0),
        put_assoc(X,Env0,Val,Env1).

execObj(Env,IP,Code,Env) :-
        max_assoc(Code,K,_), IP > K, !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% execObj to process all instructions starting from IP
execObj(EnvIn,IP,Code,EnvOut) :-
        get_assoc(IP,Code,Instr),
        (   Instr = (goto L), !, evalExpr(L,IPnext,EnvIn), EnvAux = EnvIn
        ;   Instr = (if E goto L), evalExpr(E,Val,EnvIn), EnvAux = EnvIn,
                  ( Val = 1
                    -> evalExpr(L,IPnext,EnvIn)
                    ;  IPnext is IP + 1 )
        ;   execTai(EnvIn,Instr,EnvAux), IPnext is IP+1 ),
        execObj(EnvAux,IPnext,Code,EnvOut).        

execObj(Env,IP,Code,Env,_) :-
        max_assoc(Code,K,_), IP > K, !.

% execObj modified to process one instruction

execObj(EnvIn,IP,Code,EnvOut,IPnext) :-
        get_assoc(IP,Code,Instr),
        (   Instr = (goto L), !, evalExpr(L,IPnext,EnvIn), EnvOut = EnvIn
        ;   Instr = (if E goto L), evalExpr(E,Val,EnvIn), EnvOut = EnvIn,
                  ( Val = 1
                    -> evalExpr(L,IPnext,EnvIn)
                    ;  IPnext is IP + 1 )
        ;   execTai(EnvIn,Instr,EnvOut), IPnext is IP+1 ).

execTAC(EnvIn,Code,EnvOut) :- tacToObj(Code,Obj), execObj(EnvIn,0,Obj,EnvOut,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


% Main compiler predicate, ignores the result argument
compileHL(Stmt,Code) :-
   compile(Stmt,Code,_Res).

% Test compiler
:- resetnewvar, resetnewlabel.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

debug(Code) :- 
  isStmt(Code),
  writeln('======================================='),
  writeln('Code tested:'), writeln(Code),
  compileHL(Code,Tac), writeTac(Tac),
  empty_assoc(Empty),
  execHL(Code,Empty,_),
  tacToObj(Tac,Obj),
  writeObjectcode(Obj, 0),                         % Write the first line of object code
  write('Debugger cmd: '), read(Command),
  debug(Obj, Command, Empty, 0, []).               % Handle the command

% Handle step command
debug(Obj,step,EnvIn,IP,HistList) :- 
  execObj(EnvIn,IP,Obj,EnvOut,IPNext),             % Execute the line before
  writeObjectcode(Obj, IPNext),                    % Write the line before it is executed
  write('Debugger cmd: '), read(NextCommand),      % Wait for input
  append([HistList, [IP]], HistListNew),           % Save the instruction history
  (length(HistListNew,Len), Len > 5 ->             % Make sure only 5 instructions kept
    (
      HistListNew = [_|T], 
      debug(Obj,NextCommand,EnvOut,IPNext,T)
    ) ; 
    debug(Obj,NextCommand,EnvOut,IPNext,HistListNew)).
  
% Handle inspect command
debug(Obj,inspect(E),EnvIn,IP,HistList) :- 
  get_assoc(E,EnvIn,Val),                          % Get the association
  write(E),write('='),writeln(Val),                % Print the association
  write('Debugger cmd: '), read(NextCommand),      % Wait for input
  debug(Obj,NextCommand,EnvIn,IP,HistList).        % Environment doesn't change

% Handle continue command - Simply 
debug(Obj,continue,EnvIn,IP, _) :- 
  execObj(EnvIn,IP,Obj,EnvOut),                    % Executes the rest of the function
  writeln(EnvOut).

% Handle list
debug(Obj,list,EnvIn,IP, HistList) :- 
  print_down(Obj,IP,HistList),                     % Print 5 instructions before
  writeObjectcode(Obj,IP),                         % Print current instruction
  IPNext is IP+1,
  print_up(Obj,IPNext,5),                          % Print 5 instructions after
  write('Debugger cmd: '), read(NextCommand),      % Wait for input
  debug(Obj,NextCommand,EnvIn,IP,HistList).        % Environment doesn't change

% Handle all other commands
debug(Obj,InvalidCmd,EnvIn,IP, HistList) :- 
  write('Invalid Command: '), writeln(InvalidCmd),
  write('Debugger cmd: '), read(NextCommand),      % Wait for input
  debug(Obj,NextCommand,EnvIn,IP,HistList).        % Environment doesn't change


print_down(_,_,[]).

% Simply print the list containing the history of instructions
print_down(Obj,IP,[H|T]) :-
  writeObjectcode(Obj,H),
  print_down(Obj,IP,T).
  
print_up(_, _, 0).
print_up(Obj, IP, N) :-
  N1 is N-1, N1 >= 0,
  IPNext is IP+1,
  writeObjectcode(Obj, IP),!,
  print_up(Obj, IPNext, N1), !.


:- Code = (
          x = 144 ;
          y = 60 ;
          while ( x \= y ) do {
             if ( x < y ) then {
                y = y - x ;
             } else {
                x = x - y ;
             } ;
          } ;
          ),
  debug(Code).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







