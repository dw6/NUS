package oPL;

import java.util.*;

public class LookupApplication extends Application
{

	public Expression operator;

	public Vector<Expression> operands;

	public LookupApplication(Expression rator, Vector<Expression> rands)
	{
		super(rator, rands);
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
