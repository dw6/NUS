package cVML;

public class JOF extends INSTRUCTION
{
	public int ADDRESS;

	public JOF()
	{
		OPCODE = OPCODES.JOF;
		ADDRESS = 0;
	}

	public String toString()
	{
		return "JOF" + " " + ADDRESS;
	}

	public String toXML()
	{
		return "<rvm:JOF>" + ADDRESS + "</rvm:JOF>";
	}
}
