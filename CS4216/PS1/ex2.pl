% Exercise 2 [6 marks]
% Write a program that solves systems of linear equations. 
% Your program should allow for the following type of interaction:
% ?- solve_lin( [ X*2+Y=3, 3*Y-2*X=1 ] ). X=1
% Y=1
% Yes

% Your program needs to operate under the following assumptions:
% • The system may have any number of variables, and any number of equations.
% • You only need to return a solution when the solution is unique; an error may be issued otherwise.
% • Variables may appear in an equation in any order.
% • Each variable will appear at most once in each equation.
% • The only operators appearing in equations are *, +, and - ; there will be no brackets.
% • The right hand side of every equation is a constant.
% • Every product has only two factors, and is between a constant and a variable.