package imPL;

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

	// //////////////////////
	// Denotational Semantics
	// //////////////////////

	// stub to be replaced by proper implementation

	public StoreAndValue eval(Store s, Environment e)
	{
		
		
		FunValue funValue = (FunValue)s.get(e.access(operator.toString()));
		
		int newLoc;
		
		for(int i=0; i<operands.size(); i++)
		{
			StoreAndValue s_and_v = operands.get(i).eval(s, e);
			newLoc = s_and_v.store.newLocation();
			s = s_and_v.store.extend(newLoc, s_and_v.value);
			funValue.environment = funValue.environment.extend(funValue.formals.get(i), newLoc);
		}

		return funValue.body.eval(s, funValue.environment);		
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
