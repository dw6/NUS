package imPL;

import java.util.*;

public class Let implements Expression
{

	public Vector<LetDefinition> definitions;

	public Expression body;

	public Let(Vector<LetDefinition> ds, Expression b)
	{
		definitions = ds;
		body = b;
	}

	// //////////////////////
	// Denotational Semantics
	// //////////////////////

	// stub to be replaced by proper implementation

	public StoreAndValue eval(Store s, Environment e)
	{
		for(LetDefinition ld : definitions)
		{

		}
		
		StoreAndValue s_and_v1 = body.eval(s, e);
		
		
		return new StoreAndValue(s, new BoolValue(true));
	}

	// //////////////////////
	// Support Functions
	// //////////////////////

	public String toString()
	{
		String s = "";
		for (LetDefinition d : definitions)
			s = s + " " + d;
		return "let " + s + " in " + body + " end";
	}
}
