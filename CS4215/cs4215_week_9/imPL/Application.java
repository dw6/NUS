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
//		StoreAndValue s_and_v1 = operator.eval(s, e);
//		StoreAndValue s_and_v2 = null;
//		StoreAndValue prev_s_and_v = s_and_v1;
//		
//		for(Expression e2: operands)
//		{
//			s_and_v2 = e2.eval(prev_s_and_v.store, e);
//			prev_s_and_v = s_and_v2;
//			
//		}
//		
//		FunValue f = (FunValue)s_and_v1.value;
		
		return new StoreAndValue(s, new BoolValue(true));
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
