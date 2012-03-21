package imPL;

public class Assignment implements Expression
{
	public String leftHandSide;
	public Expression rightHandSide;

	public Assignment(String l, Expression r)
	{
		leftHandSide = l;
		rightHandSide = r;
	}

	// //////////////////////
	// Denotational Semantics
	// //////////////////////

	// stub to be replaced by proper implementation

	public StoreAndValue eval(Store s, Environment e)
	{
		StoreAndValue s_and_v1 = rightHandSide.eval(s, e);
		Value v = s_and_v1.value;
		return new StoreAndValue(s.extend(e.access(leftHandSide), v), v);
	}

	// //////////////////////
	// Support Functions
	// //////////////////////

	public String toString()
	{
		return "(" + leftHandSide + " := " + rightHandSide + ")";
	}
}
