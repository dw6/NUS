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


	public Type check(TypeEnvironment G) throws TypeError
	{
		System.err.println("Checking #Function# type");

		// Extend the environment with function input arguments
		G = G.extend(formals,((FunType)funType).argumentTypes);

		if (((FunType)funType).returnType.toString().equals(body.check(G).toString()))
		{
			return new FunType(((FunType)funType).argumentTypes, ((FunType)funType).returnType);
		}
		else
		{
			throw new TypeError("ill-typed function " + this);
		}
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
