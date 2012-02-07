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

	public Type check(TypeEnvironment G) throws TypeError
	{				
		// The type of the operator needs to be a function type with the right number of parameters 
		Type operatorType = operator.check(G);
		
		int numOperands = operands.size();
		int numParameters = ((FunType) operatorType).argumentTypes.size();
		
		if (operatorType instanceof FunType && numOperands == numParameters)
		{
			for(int i = 0; i < ((FunType) operatorType).argumentTypes.size(); i++)
			{
				// The type of every argument needs to coincide with the 
				// corresponding parameter type of the function type. 				
				if(!EqualType.equalType(((FunType) operatorType).argumentTypes.get(i), operands.get(i).check(G)))
				{
					throw new TypeError("");
				}
			}
			
			// If all these conditions are met, the type of the function application 
			// is the same as the return type of the function type that is the type of the operator.			
			return ((FunType) operatorType).returnType;
			

		}
		else
		{
			throw new TypeError("");
		}
	
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
