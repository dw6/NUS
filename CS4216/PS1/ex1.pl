% Benjamin Tan Wei Hao
% U077129N
% PS1, Exercise 1

:- lib(hash). % query "help hash." to find contents of library
:- local op(948,fx,print).
:- local op(1099,xf,;).
:- local op(950,fxx,if).
:- local op(949,fxx,while).
:- local op(949,xfx,else).

% Expression evaluator
eval(X,SymTab,Result) :- atom(X), !, hash_get(SymTab,X,Result).
eval(X,_,X) :- number(X), !.

% Arithmetic Operators
eval(E,SymTab,Result) :- 
	E =.. [Op,X,Y], member(Op,[+,-,*,/, <<, >>]), !, 
	eval(X,SymTab,Rx), eval(Y,SymTab,Ry), Z =.. [Op,Rx,Ry],
	Result is Z.
	
% Comparison Operators
eval(E,SymTab,Result) :- 
	E =.. [Op,X,Y], member(Op,[<,>,=<,>=,==,\=]), !, 
	eval(X,SymTab,Rx), eval(Y,SymTab,Ry), Z =.. [Op,Rx,Ry],
	(Z -> Result is 1 ; Result is 0).
      
% Execution engine
execp((X = E), SymTab) :- !, 
  atom(X), eval(E,SymTab,Result), hash_set(SymTab,X,Result).
execp((print X), SymTab) :- !, 
  (  atom(X), not hash_contains(SymTab,X) 
  -> write(X) 
  ;  eval(X,SymTab,Result), write(Result) ).
execp( nl, _ ) :- !, nl.

execp( (if Cond Conseq else Alternative), SymTab ) :- !, 
  eval(Cond,SymTab,B),
  (   B \= 0 
  ->  execp(Conseq,SymTab) 
  ;   execp(Alternative,SymTab) ).

execp( (if Cond Conseq ), SymTab ) :- !, 
  eval(Cond,SymTab,B), 
  (   B \= 0 
  ->  execp(Conseq,SymTab) 
  ;   true ).

% Evaluate the condition. If true, call the loop again, with the updated
% Symbol Table.
execp( (while Cond WhileBody), SymTab) :- !,
	eval(Cond,SymTab,B),
  (   B \= 0 ->  
  	execp(WhileBody,SymTab), execp( (while Cond WhileBody),SymTab) ; true ) .

execp( ( Stmt1 ; Stmt2 ), SymTab ) :- !, 
  execp(Stmt1,SymTab), execp(Stmt2,SymTab).
execp( { Stmt ; }, SymTab ) :- !, execp(Stmt,SymTab).
execp( { Stmt }, SymTab ) :- !, execp(Stmt,SymTab).
execp( (Stmt ;), SymTab ) :- execp(Stmt,SymTab).


% Sample query for interpreter
%    will print "z=3\ndone\n"
:- Program = (
     x = 0 ;
     y = 1 ;

     while (x < 10) {
       x = x + 1 ;
       y = y * 10 ;
     } ;
     print 'y=' ;
     print y  ;
     nl ;
     print done ;
     nl ;
   ), hash_create(SymTab), execp(Program, SymTab).

     % x = 10 ;
     % y = -10 ;
     % if (x \= y) {
     %   z = 2 ;
     %   z = z + 1 ;
     % } else {
     %   z = 100 ;
     %   z = z + x ;
     % } ;
     % print 'z=' ;
     % print z  ;
     % nl ;
     % print done ;
     % nl ;