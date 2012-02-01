/**
 * Benjamin Tan Wei Hao U077129N
 */


package ePLdynamic;

import ePL.*;

class Evaluator
{
	// evaluate repeatedly implements the evaluation relation
	// by repeatedly calling oneStep until the expression is
	// not reducible
	static public Expression evaluate(Expression exp)
	{
		if (reducible(exp))
		{
			return evaluate(oneStep(exp));
		}
		else
		{
			if(exp.toString().contains("Error"))
			{
				/**
				 * I chose not to implemented this as an exception, and instead 
				 * return an error message.
				 */
				return new BoolConstant("Error: Most probably division by zero.");
			}
			else
			{
				return exp;
			}
		}		
	}

	// in ePL, a reducible expression is a PrimitiveApplication that
	// has a reducible expression as one of its arguments, or that has constants
	// of the right type as arguments

	static private boolean reducible(Expression exp)
	{
		if (exp instanceof BinaryPrimitiveApplication)
		{
			return reducible(((BinaryPrimitiveApplication) exp).argument1)
					|| reducible(((BinaryPrimitiveApplication) exp).argument2) ||

					// integer ops without division
					((((BinaryPrimitiveApplication) exp).operator.equals("+")
							|| ((BinaryPrimitiveApplication) exp).operator.equals("*") || ((BinaryPrimitiveApplication) exp).operator
								.equals("-"))
							&& ((BinaryPrimitiveApplication) exp).argument1 instanceof IntConstant
							&& ((BinaryPrimitiveApplication) exp).argument2 instanceof IntConstant ||
					// division (exclude second argument 0)
					((BinaryPrimitiveApplication) exp).operator.equals("/")
							&& ((BinaryPrimitiveApplication) exp).argument1 instanceof IntConstant
							&& ((BinaryPrimitiveApplication) exp).argument2 instanceof IntConstant) ||
					// boolean cases - AND/OR
					((((BinaryPrimitiveApplication) exp).operator.equals("&") || 
					  ((BinaryPrimitiveApplication) exp).operator.equals("|")) && 
					  ((BinaryPrimitiveApplication) exp).argument1 instanceof BoolConstant && 
					  ((BinaryPrimitiveApplication) exp).argument2 instanceof BoolConstant) ||
					// comparison operators - < and >
					((((BinaryPrimitiveApplication) exp).operator.equals("<") || 
					  ((BinaryPrimitiveApplication) exp).operator.equals(">") || 
					  ((BinaryPrimitiveApplication) exp).operator.equals("=")) && 
					  ((BinaryPrimitiveApplication) exp).argument1 instanceof IntConstant && 
					  ((BinaryPrimitiveApplication) exp).argument2 instanceof IntConstant);  
					  
		}
		else if (exp instanceof UnaryPrimitiveApplication) 
		{
			return (((UnaryPrimitiveApplication) exp).operator.equals("\\")) &&
				   (((UnaryPrimitiveApplication) exp).argument instanceof BinaryPrimitiveApplication ||
				   ((UnaryPrimitiveApplication) exp).argument instanceof UnaryPrimitiveApplication   ||
				   ((UnaryPrimitiveApplication) exp).argument instanceof BoolConstant);
		}
		else
		{
			return false;
		}
	}

	// oneStep finds the place where a given expression is
	// reducible, and carries out the reduction at that place

	static private Expression oneStep(Expression exp)
	{
		// System.err.println(exp.toString());
		
		if (exp instanceof UnaryPrimitiveApplication)
		{
			Expression arg = ((UnaryPrimitiveApplication) exp).argument;
			String operator = ((UnaryPrimitiveApplication)exp).operator;
			
			if (((UnaryPrimitiveApplication)exp).operator.equals("\\"))
			{				
				if (reducible(arg))
				{
					return new UnaryPrimitiveApplication(operator, oneStep(arg));
				}
				else
				{
					return new BoolConstant(Boolean.toString(!((BoolConstant) arg).value.equals("true")));
				}		
			}
			else 
			{
				System.err.println("Unrecognized Unary operator.");
				return null;
			}
		}
		else
		{
			String operator = ((BinaryPrimitiveApplication) exp).operator;
			Expression firstArg = ((BinaryPrimitiveApplication) exp).argument1;
			Expression secondArg = ((BinaryPrimitiveApplication) exp).argument2;
			
			
			if (firstArg.toString().contains("null") || secondArg.toString().contains("null")) 
			{
				return new UnaryPrimitiveApplication("", new BoolConstant("Error"));
			}
			else if (reducible(firstArg))
			{
				return new BinaryPrimitiveApplication(operator, oneStep(firstArg), secondArg);
			}
			else if (reducible(secondArg))
			{
				return new BinaryPrimitiveApplication(operator, firstArg, oneStep(secondArg));
			}
			else if (operator.equals("+"))
			{
				return new IntConstant(Integer.toString(Integer
						.parseInt(((IntConstant) firstArg).value)
						+ Integer.parseInt(((IntConstant) secondArg).value)));
			}
			else if (operator.equals("-"))
			{
				return new IntConstant(Integer.toString(Integer
						.parseInt(((IntConstant) firstArg).value)
						- Integer.parseInt(((IntConstant) secondArg).value)));
			}
			else if (operator.equals("*"))
			{
				return new IntConstant(Integer.toString(Integer
						.parseInt(((IntConstant) firstArg).value)
						* Integer.parseInt(((IntConstant) secondArg).value)));
			}
			else if (operator.equals("/"))
			{
				int numerator 	= Integer.parseInt(((IntConstant) firstArg).value);
				int denominator = Integer.parseInt(((IntConstant) secondArg).value); 
				
				if (denominator != 0) {
					return new IntConstant(Integer.toString(numerator / denominator));
				}
				else
				{
					/*
					 * Instead of blowing up, return BoolConsant, with an "Error" to 
					 * mark the result as an error.
					 */
					return new UnaryPrimitiveApplication("", new BoolConstant("Error"));
				}
			}
			else if (operator.equals("&"))
			{
				
				return new BoolConstant( Boolean.toString( 
										 Boolean.parseBoolean(((BoolConstant)firstArg).value) && 
										 Boolean.parseBoolean(((BoolConstant)secondArg).value))
				);
			}
			else if (operator.equals("|"))
			{
				return new BoolConstant( Boolean.toString( 
						 				 Boolean.parseBoolean(((BoolConstant)firstArg).value) || 
						 				 Boolean.parseBoolean(((BoolConstant)secondArg).value))
				);
			}
			else if (operator.equals("<"))
			{
				return new BoolConstant(Boolean.toString(
									    Integer.parseInt(((IntConstant) firstArg).value) < 
									    Integer.parseInt(((IntConstant) secondArg).value))
				);
			}
			else if (operator.equals(">"))
			{
				return new BoolConstant(Boolean.toString(
						   				Integer.parseInt(((IntConstant) firstArg).value) > 
						   				Integer.parseInt(((IntConstant) secondArg).value))
				);
			}
			else if (operator.equals("="))
			{
				return new BoolConstant(Boolean.toString(
						   				Integer.parseInt(((IntConstant) firstArg).value) == 
						   				Integer.parseInt(((IntConstant) secondArg).value))
				);
			}
			else
			{
				return new IntConstant(Integer.toString(Integer.MAX_VALUE));
			}
		}
	}
}
