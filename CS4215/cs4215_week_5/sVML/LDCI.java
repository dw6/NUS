package sVML;

/**
 * Load Constant Integer
 *
 */

public class LDCI extends INSTRUCTION
{
	private static final long serialVersionUID = 1L;
	public int VALUE;

	/**
	 * LDCI - Load Constant integer
	 * @param i
	 */
	public LDCI(int i)
	{
		OPCODE = OPCODES.LDCI;
		VALUE = i;
	}

	public String toString()
	{
		return "LDCI" + " " + VALUE;
	}

	public String toXML()
	{
		return "<svm:LDCI>" + VALUE + "</svm:LDCI>";
	}
}
