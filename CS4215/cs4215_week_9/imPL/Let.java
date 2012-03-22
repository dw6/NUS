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
		StoreAndValue s_and_v1 = null;
		for(LetDefinition ld : definitions)
		{
			s_and_v1 = ld.rightHandExpression.eval(s, e);
			int oldStoreLocation = s.newLocation();
			s = s.extend(oldStoreLocation, s_and_v1.value);
			e = e.extend(ld.variable, oldStoreLocation);
		}
		
		StoreAndValue s_and_v2 = body.eval(s, e);
		return new StoreAndValue(s_and_v2.store, s_and_v2.value);
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
