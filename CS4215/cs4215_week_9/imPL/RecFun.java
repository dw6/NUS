package imPL;

import java.util.*;

public class RecFun extends Fun
{

	public String funVar;

	public RecFun(String f, Vector<String> xs, Expression b)
	{
		super(xs, b);
		funVar = f;
	}

	// //////////////////////
	// Denotational Semantics
	// //////////////////////

	// stub to be replaced by proper implementation

	public StoreAndValue eval(Store s, Environment e)
	{
		System.err.println("Store : " + s);
		System.err.println("Envir : " + e);
		System.err.println("FunVar: " + funVar);
		System.err.println("Formal: " + formals);
		System.err.println("Body  : " + body);
		return new StoreAndValue(s, new BoolValue(true));
	}

	// //////////////////////
	// Support Functions
	// //////////////////////

	public String toString()
	{
		String s = "";
		for (String f : formals)
			s = s + " " + f;
		return "recfun " + funVar + s + " -> " + body + " end";
	}
}
