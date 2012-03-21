package imPL;

import java.util.*;

public class Application implements Expression {

	public Expression operator;

	public Vector<Expression> operands;

	public Application(Expression rator,Vector<Expression> rands) {
		operator = rator; operands = rands;
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
		for  (Expression operand : operands) 
			s = s + " " + operand;
		return "("+operator+" "+s+")";
	}
}
