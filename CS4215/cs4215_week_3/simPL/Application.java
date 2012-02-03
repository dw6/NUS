package simPL;

import java.util.*;

public class Application implements Expression
{
	public Expression operator;

	public Vector<Expression> operands;

	public Application(Expression rator, Vector<Expression> rands)
	{
		operator = rator;
		operands = rands;
	}

	public Expression eliminateLet()
	{		
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

	public Expression substitute(String var, Expression replacement)
	{		
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
		System.err.println(this);
		return operator.reducible() || isOperandsReducible() || (operator instanceof Fun && !isOperandsReducible()); 
//				|| (operator instanceof Application && isOperandsReducible());
	}

	public Expression oneStep()
	{
		
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
			if (operator instanceof RecFun)
			{
				System.err.println("# RecFun Application #");
				RecFun operator = ((RecFun) this.operator);
				Expression body = operator.body;
				
				// Substitute occurrences of function name with the function
				body = body.substitute(operator.funVar, operator);
								
				// When we are here, we are sure that the form is
				// (fun ... end Op1 ... OpN)
				Vector<String> formals = operator.formals;
				
				for(int i=0; i<formals.size(); i++) 
				{
					body = body.substitute(formals.get(i), operands.get(i));	
				}			

				return body.oneStep();					
			}
			else if(operator instanceof Fun)
			{
				System.err.println("# Fun Application #");
				Fun operator = ((Fun) this.operator);
				Expression body = operator.body;
							
				// When we are here, we are sure that the form is
				// (fun ... end Op1 ... OpN)
				Vector<String> formals = operator.formals;
				
				for(int i=0; i<formals.size(); i++) 
				{
					body = body.substitute(formals.get(i), operands.get(i));	
				}			
							
				return body;				
			}

			// Shouldn't get to here
			return this;
		}
		
	}

	// //////////////////////
	// Support Functions
	// //////////////////////
	
	// Checks if one of the operands is reducible
	public boolean isOperandsReducible() {
		for(Expression o : operands) {
			if(o.reducible() == false) {
				return false;
			}
		}
		return true;
	}

//	public boolean isOperandsReducible() {
//		for(Expression o : operands) {
//			if(o.reducible()) {
//				return true;
//			}
//		}
//		return false;
//	}
	
	public Vector<Expression> reduceOperandsByOneStep() 
	{
		Vector<Expression> new_operands = operands;
		
		for(Expression o : operands) {
			if (o.reducible()) {
				new_operands.add(o.oneStep());
			}
			else
			{
				return new_operands;
			}
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