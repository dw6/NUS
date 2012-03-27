package oPL;

public class Wrapper
{
	public static String prologue = 
		"let new = fun theClass -> [Class: theClass] end " +
		"	 lookup = fun object methodName -> object.Class.methodname end" +		
		" in ";



					
	
	public static String epilogue = " end";
}
