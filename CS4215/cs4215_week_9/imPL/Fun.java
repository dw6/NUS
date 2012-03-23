package imPL;

import java.util.*;

public class Fun implements Expression
{

	public Vector<String> formals;
	public Expression body;

	public Fun(Vector<String> xs, Expression b)
	{
		formals = xs;
		body = b;
	}

	// //////////////////////
	// Denotational Semantics
	// //////////////////////

	// stub to be replaced by proper implementation

	public StoreAndValue eval(Store s, Environment e)
	{
		// Create the environment . Make space for the formals into this environment.
		Environment funEnv = new Environment();

		// Using the existing store, assign all the formals a location inside this store.
		int newLoc;
		for(String formal: formals)
		{
			newLoc = s.newLocation();
			funEnv = funEnv.extend(formal, newLoc);
			// Initialize the location to null. 
			// This will be filled in by the Application.
			s = s.extend(newLoc, null);
		}	
		return new StoreAndValue(s, new FunValue(funEnv, formals, body));
	}

	// //////////////////////
	// Support Functions
	// //////////////////////

	public String toString()
	{
		String s = "";
		for (String f : formals)
			s = s + " " + f;
		return "fun" + s + " -> " + body + " end";
	}
}
