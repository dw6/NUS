package imPL;

public class Sequence implements Expression
{
	public Expression firstPart, secondPart;

	public Sequence(Expression f, Expression s)
	{
		firstPart = f;
		secondPart = s;
	}

	// //////////////////////
	// Denotational Semantics
	// //////////////////////

	// stub to be replaced by proper implementation

	public StoreAndValue eval(Store s, Environment e)
	{
		StoreAndValue s_and_v1 = firstPart.eval(s, e);
		StoreAndValue s_and_v2 = secondPart.eval(s_and_v1.store, e);
		return new StoreAndValue(s_and_v2.store, s_and_v2.value);
	}

	// //////////////////////
	// Support Functions
	// //////////////////////

	public String toString()
	{
		return "(" + firstPart + " ; " + secondPart + ")";
	}
}
