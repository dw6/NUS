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
		StoreAndValue s_and_v = super.eval(s, e);
		int newLoc = s_and_v.store.newLocation();
		s_and_v.store = s_and_v.store.extend(newLoc, s_and_v.value);
		((FunValue) s_and_v.value).environment = ((FunValue) s_and_v.value).environment.extend(
				funVar, newLoc);
		return new StoreAndValue(s_and_v.store, new FunValue(
				((FunValue) s_and_v.value).environment, super.formals, super.body));
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
