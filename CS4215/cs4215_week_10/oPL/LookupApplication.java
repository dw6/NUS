package oPL;

import java.util.*;

public class LookupApplication extends Application
{
	public String operator;

	public Vector<Expression> operands;

	public LookupApplication(Expression rator, Vector<Expression> rands)
	{
		super(rator, rands); // Seems like no effect.
		operator = rator.toString();
		operands = rands;
	}
	
	public Value eval(Environment e)
	{
		System.err.println(" ** Evaluating Lookup Application ** ");
		System.err.println("[ Operator ] > " + this.operator);
		System.err.println("[ Operands ] > " + this.operands);
		System.err.println("[ Environment ]");
		System.err.println(e);
		System.err.println("[ Store ]");
		System.err.println(Store.theStore.toString());
		
		int i = 0;
		for(Value v : Store.theStore) 
		{
			System.err.println(i++ + ") " +  v);
		}
		
		// The first operand is always "this"
		// Locate the address of the object.
		String thisObj = operands.get(0).toString();
		int objAddr = e.access(thisObj);

		// Get the objClass from the store.
		RecordValue objRecord = (RecordValue)Store.theStore.get(objAddr);
		int objMethodAddr = objRecord.get("Class");
		
		RecordValue objMethodRecord = (RecordValue)Store.theStore.get(objMethodAddr);
		// Access the method
		System.err.println(objMethodRecord);
		
		return objMethodRecord;
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
