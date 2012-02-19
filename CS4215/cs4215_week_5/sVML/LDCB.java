package sVML;

public class LDCB extends INSTRUCTION
{
	/**
	 * Load Constant Boolean
	 */
	private static final long serialVersionUID = 1L;
	public boolean VALUE;

	public LDCB(boolean b)
	{
		OPCODE = OPCODES.LDCB;
		VALUE = b;
	}

	public String toString()
	{
		return "LDCB" + " " + VALUE;
	}

	public String toXML()
	{
		return "<svm:LDCB>" + VALUE + "</svm:LDCB>";
	}
}
