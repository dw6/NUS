package simPL;

import java.util.*;

public class Application implements Expression
{
	public Expression operator;

	public Vector<Expression> operands;

	public Application(Expression rator, Vector<Expression> rands)
	{
		System.err.println("# Application #");		
		operator = rator;
		operands = rands;
	}

	public Expression eliminateLet()
	{
		System.err.println("Eliminate let");		
		
		Vector<Expression> new_operands = new Vector<Expression>();
		for(Expression o : operands) 
		{
			new_operands.add(o.eliminateLet());
		}
		return new Application(operator.eliminateLet(), new_operands);
	}

	// //////////////////////
	// Dynamic Semantics
	// //////////////////////

	public StringSet freeVars()
	{		
		return operator.freeVars();
	}

	// stub; to be replaced by student
	public Expression substitute(String var, Expression replacement)
	{
		System.err.println("Substitute in Application");		
		
		Vector<Expression> new_operands = new Vector<Expression>();
		for(Expression o : operands) 
		{
			new_operands.add(o.substitute(var, replacement));
		}
		
		return new Application(operator.substitute(var, replacement), new_operands);	
	}
	
		
	// Q: When is Function Application reducible?
	// A: When either its operands or operator is reducible
	public boolean reducible()
	{		
		System.err.println("# Reducible #: " + (operator.reducible() || isOperandsReducible() || (operator instanceof Fun && !isOperandsReducible())));
		return operator.reducible() || isOperandsReducible() || (operator instanceof Fun && !isOperandsReducible()) || operator instanceof Application;
	}

	public Expression oneStep()
	{
		System.err.println("One Step called");
		if (operator.reducible())
		{
			return new Application(operator.oneStep(), operands);
		}
		else if(isOperandsReducible())
		{
			return new Application(operator, reduceOperandsByOneStep());
		}
		else 
		{
			// When we are here, we are sure that the form is
			// (fun ... end Op1 ... OpN)
			Expression body = ((Fun)operator).body;
			Vector<String> formals = ((Fun) operator).formals;
			
			for(int i=0; i<formals.size(); i++) 
			{
				body = body.substitute(formals.get(i), operands.get(i));	
			}
						
			return body;
		}
		
	}

	// //////////////////////
	// Support Functions
	// //////////////////////
	
	// Checks if one of the operands is reducible
	public boolean isOperandsReducible() {
		for(Expression o : operands) {
			if(o.reducible()) return true;
		}
		return false;
	}

	public Vector<Expression> reduceOperandsByOneStep() 
	{
		Vector<Expression> new_operands = new Vector<Expression>();
		
		for(Expression o : operands) {
			new_operands.add(o.oneStep());
		}
		return new_operands;
	}
	

	public String toString()
	{
		String s = "";
		for (Expression operand : operands)
			s = s + " " + operand;
		return "(" + operator + " " + s + ")";
	}

	public String toXML()
	{
		String s = "";
		for (Expression operand : operands)
			s = s + operand.toXML();
		return "<simpl:application>\n" + "<simpl:operator>\n" + operator.toXML()
				+ "</simpl:operator>\n" + "<simpl:arguments>\n" + s + "</simpl:arguments>\n"
				+ "</simpl:application>\n";
	}
}