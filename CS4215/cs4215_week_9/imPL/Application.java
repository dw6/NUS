package imPL;

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

	// //////////////////////
	// Denotational Semantics
	// //////////////////////

	// stub to be replaced by proper implementation

	public StoreAndValue eval(Store s, Environment e)
	{
		assert (operator instanceof Fun);

		if (operator instanceof RecFun)
		{

		}
		else if (operator instanceof Fun)
		{
			// 1. Evaluate the operator.
			StoreAndValue s_and_v1 = operator.eval(s, e);
			FunValue funValue = (FunValue) s_and_v1.value;
			Environment funEnv = funValue.environment;
			
			for (int i = 0; i < operands.size(); i++)
			{
				System.err.println("");
				
				// 2. Fill up the space with values.
				s_and_v1.store.setElementAt(operands.get(i).eval(s, e).value, funEnv
						.access(funValue.formals.get(i)));
				
			    System.err.println("Here");

			}

			// 3. Evaluate the function.
						
			// Iterate through the "Bigger" environment, 
			// add in anything which doesn't exists in the function environment.
		    Enumeration<String> envKeys = e.keys();
		    		    
		    while(envKeys.hasMoreElements()) {
		      String elt = envKeys.nextElement().toString();
		      if (!funEnv.containsKey(elt))
		      {
		    	  funEnv = funEnv.extend(elt, e.access(elt));
		      }
		  	}
		    
		    
			return funValue.body.eval(s_and_v1.store, funEnv);
		} 
		else
		{
			// Recast everything, and call Application again
			FunValue funValue = (FunValue)s.get(e.access(operator.toString()));
			Fun function = new Fun(funValue.formals, funValue.body);
			Application application = new Application(function, operands);
			return application.eval(s, e);
		}

		return null;
	}

	// //////////////////////
	// Support Functions
	// //////////////////////

	public String toString()
	{
		String s = "";
		for (Expression operand : operands)
			s = s + " " + operand;
		return "(" + operator + " " + s + ")";
	}
}
