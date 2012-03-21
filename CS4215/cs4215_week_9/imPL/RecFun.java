package imPL;

import java.util.*;

public class RecFun extends Fun {

	public String funVar;

	public RecFun(String f, Vector<String> xs, Expression b) {
		super(xs,b);
		funVar = f;
	}

	// //////////////////////
	// Denotational Semantics
	// //////////////////////

	// stub to be replaced by proper implementation

	public StoreAndValue eval(Store s, Environment e) {
		return new StoreAndValue(s,new BoolValue(true));
	}

	// //////////////////////
	// Support Functions
	// //////////////////////

	public String toString() {
		String s = "";
		for (String f : formals)
			s = s + " " + f;
		return "recfun " + funVar + 
				s + " -> " + body + " end";
	}
}
