package simPL;

import java.util.*;

public class Fun implements Expression
{

	public Type funType;
	public Vector<String> formals;
	public Expression body;

	public Fun(Type t, Vector<String> xs, Expression b)
	{
		funType = t;
		formals = xs;
		body = b;
	}

	public Expression eliminateLet()
	{
		return new Fun(funType, formals, body.eliminateLet());
	}

	// to be implemented by student

	public Type check(TypeEnvironment G) throws TypeError
	{
		return null;
	}

	// //////////////////////
	// Support Functions
	// //////////////////////

	public String toString()
	{
		String s = "";
		for (String f : formals)
			s = s + " " + f;
		return "fun {" + funType + "}" + s + " -> " + body + " end";
	}
}
