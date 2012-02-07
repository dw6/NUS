package simPL;

import java.util.*;

public class RecFun extends Fun
{

	public String funVar;

	public RecFun(String f, Type t, Vector<String> xs, Expression b)
	{
		super(t, xs, b);
		funVar = f;
	}

	public Expression eliminateLet()
	{
		return new RecFun(funVar, funType, formals, body.eliminateLet());
	}

	// to be implemented by student

	public Type check(TypeEnvironment G) throws TypeError
	{
		// Need to check first that the formal arguments is the same length as the input parameters
		if (((FunType)funType).argumentTypes.size() != formals.size())
		{
			throw new TypeError("");
		}
		else
		{
			// Here, we find a type t for the body of the function under the assumption that
			// the function identifier has the type that is declared for the function.

			// Thus for a function definition to be well-typed under the assumptions given by
			// type environment G1, the body of the function needs to be well-typed under the
			// assumptions given by an extended environment Gn+1, where Gn+1 extends G1
			// with bindings of the functionÕs formal parameters to its declared types. 
			
			// Extend the environment with function definition and function input arguments
			G = G.extend(this.funVar, ((FunType)funType)).extend(formals,((FunType)funType).argumentTypes);
		
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
		return "recfun " + funVar + " {" + funType + "} " + s + " -> " + body + " end";
	}
}
