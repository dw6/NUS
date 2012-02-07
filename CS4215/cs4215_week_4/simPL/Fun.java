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
		// Need to check first that the formal arguments is the same length as the input parameters
		if (((FunType)funType).argumentTypes.size() != formals.size())
		{
			throw new TypeError("ill-typed function " + this);
		}
		else
		{
			// Thus for a function definition to be well-typed under the assumptions given by
			// type environment G1, the body of the function needs to be well-typed under the
			// assumptions given by an extended environment Gn+1, where Gn+1 extends G1
			// with bindings of the functionÕs formal parameters to its declared types. 
			G = G.extend(formals,((FunType)funType).argumentTypes);
			
			// Furthermore, the type of the body needs to coincide with the declared return type
			// of the function
			
			if (EqualType.equalType(((FunType)funType).returnType, body.check(G)))
			{
				return funType;
			}
			else
			{
				throw new TypeError("ill-typed function " + this);
			}			
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
