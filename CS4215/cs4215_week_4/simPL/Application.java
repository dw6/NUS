package simPL;

import java.util.*;

public class Application implements Expression
{

	public Expression operator;

	public Vector<Expression> operands;

	public Application(Expression rator, Vector<Expression> rands)
	{
		operator = rator;
		operands = rands;
	}

	public Expression eliminateLet()
	{
		Vector<Expression> newoperands = new Vector<Expression>();
		for (Expression operand : operands)
			newoperands.add(operand.eliminateLet());
		return new Application(operator.eliminateLet(), newoperands);
	}

	// Check RHS match LHS int * int 
	public Type check(TypeEnvironment G) throws TypeError
	{		
		System.err.println("Checking #Application# type");
		
		// Check the type of the function! 
		Type result1 = operator.check(G);
		
		if (result1 instanceof FunType)
		{
			
		}
		else
		{
			throw new TypeError("ill-typed function application " + this);
		}

		
		return null;
	}

	// //////////////////////
	// Support Functions
	// //////////////////////

	public String toString()
	{
		String s = "";
		for (Expression operand : operands)
			s = s + " " + operand;
		return "(" + operator + " " + s + ")";
	}
}
