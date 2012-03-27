package oPL;

import java.util.*;

public class LookupApplication extends Application
{

	public Expression operator;

	public Vector<Expression> operands;

	public LookupApplication(Expression rator, Vector<Expression> rands)
	{
		super(rator, rands);
		System.err.println("Yay!!!");
	}

	
	public Value eval(Environment e)
	{
		System.err.println("Huat larh! At least I'm here");
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
