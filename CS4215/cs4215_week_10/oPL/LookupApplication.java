package oPL;

import java.util.*;

public class LookupApplication extends Application
{	
	RecordValue theClass;
	FunValue theMethod;
	
	public LookupApplication(Expression rator, Vector<Expression> rands)
	{
		// The operator is ALWAYS lookup
		super(rator, rands); // Seems like no effect.
		theClass  = null;
		theMethod = null;
	}
	
	public Value eval(Environment e)
	{
		// The first operand is always "this"
		// Locate the address of the object.
		String thisObj = super.operands.get(0).toString();
		int objAddr = e.access(thisObj);
				
		// Get the objClass from the store.
		RecordValue objRecord = (RecordValue)Store.theStore.get(objAddr);		
		RecordValue objClass = (RecordValue)Store.theStore.get(objRecord.get("Class"));
		
		
		// When do we need to cache?
		if (theClass == null || theClass != objClass)
		{
			// Cache miss
			// Perform lookup and update the cache
			theMethod = (FunValue) super.eval(e);
			theClass = objClass;
			return theMethod;
		}
		else
		{
			// Cache hit
			return theMethod;
		}
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



