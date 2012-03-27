package oPL;

public class Wrapper
{
	public static String prologue = 
		"let access = fun r p -> r.p end " +
		"	 new = fun theClass -> [Class: theClass] end " +
		"	 lookupInClass = recfun lookupInClass theClass methodname ->" +
		"						if theClass hasproperty methodname" +
		"						then theClass.methodname" +
		"						else (lookupInClass theClass.Parent methodname)" +
		"						end" +
		"					  end" +
		" in " +
		"	let	lookup = fun object methodName -> object.Class.methodname end" +		
		" 	in ";
	
	public static String epilogue = " end end";
}
