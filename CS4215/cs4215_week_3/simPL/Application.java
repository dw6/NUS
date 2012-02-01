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

	// stub; to be replaced by student
	public Expression eliminateLet()
	{
		return this;
	}

	// //////////////////////
	// Dynamic Semantics
	// //////////////////////

	// stub; to be replaced by student
	public StringSet freeVars()
	{
		return new StringSet();
	}

	// stub; to be replaced by student
	public Expression substitute(String var, Expression replacement)
	{
		return this;
	}

	// stub; to be replaced by student
	public boolean reducible()
	{
		return false;
	}

	// stub; to be replaced by student
	public Expression oneStep()
	{
		return this;
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

	public String toXML()
	{
		String s = "";
		for (Expression operand : operands)
			s = s + operand.toXML();
		return "<simpl:application>\n" + "<simpl:operator>\n" + operator.toXML()
				+ "</simpl:operator>\n" + "<simpl:arguments>\n" + s + "</simpl:arguments>\n"
				+ "</simpl:application>\n";
	}
}
