package oPL;

import java.util.*;

public class LookupApplication extends Application
{
	class ClassAndMethodValue
	{
		String className  = "";
		String methodName = "";
		ClassAndMethodValue(String className, String methodName)
		{
			this.className  = className;
			this.methodName = methodName;
		}
		
		@Override
		public int hashCode()
		{
			final int prime = 31;
			int result = 1;
			result = prime * result + ((className == null) ? 0 : className.hashCode());
			result = prime * result + ((methodName == null) ? 0 : methodName.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj)
		{
			ClassAndMethodValue other = (ClassAndMethodValue) obj;
			if(this.className.equals(other.className) && this.methodName.equals(other.methodName))
			{
				return true;
			}
			return false;
		}


		private LookupApplication getOuterType()
		{
			return LookupApplication.this;
		}
	}
	
	public String operator;
	public Vector<Expression> operands;
	public static Hashtable<ClassAndMethodValue,FunValue> cache = new Hashtable<ClassAndMethodValue,FunValue>();

	// 1. Find the class of the object.
	
	
	
	// 2. Find the method name from operands
	
	
	
	public LookupApplication(Expression rator, Vector<Expression> rands)
	{
		// The operator is ALWAYS lookup
		super(rator, rands); // Seems like no effect.
		operator = rator.toString();
		operands = rands;
	}
	
	public Value eval(Environment e)
	{
		
		// The first operand is always "this"
		// Locate the address of the object.
		String thisObj = operands.get(0).toString();
		int objAddr = e.access(thisObj);
				
		// Get the objClass from the store.
		RecordValue objRecord = (RecordValue)Store.theStore.get(objAddr);		
		RecordValue theClass = (RecordValue)Store.theStore.get(objRecord.get("Class"));
						
		FunValue lookupResult = 
				cache.get(new ClassAndMethodValue(theClass.toString(), operands.get(1).toString()));
				
		FunValue theMethod = null;
				
		if(lookupResult == null)
		{
			// here we perform the actual lookup.
			// 1. First, we have to locate the function called 'lookup'
			FunValue lookupFun = (FunValue)Store.theStore.get(e.access(operator));
			
			// 2. Second, we run the actual lookup.
			
			// For this (the obj)
			int newLoc = Store.theStore.newLocation();
			Store.theStore.extend(newLoc, objRecord);
			lookupFun.environment = lookupFun.environment.extend(lookupFun.formals.firstElement(), newLoc);
			
			// For methodname
			newLoc = Store.theStore.newLocation();			
			Store.theStore.extend(newLoc, operands.get(1).eval(e));	
			lookupFun.environment = lookupFun.environment.extend(lookupFun.formals.lastElement(), newLoc);
					
			theMethod = (FunValue)lookupFun.body.eval(lookupFun.environment);
			cache.put(new ClassAndMethodValue(theClass.toString(), operands.get(1).toString()), theMethod);	
		}
		else
		{
			theMethod = lookupResult;
		}		
		return theMethod;
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



