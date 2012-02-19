package simPLvm;

public class Closure implements Value
{
	public Environment environment;
	public int ADDRESS;

	public Closure(Environment e, int a)
	{
		environment = e;
		ADDRESS = a;
	}
	
	public String toString()
	{
		return new String("env: " + environment + " addr: " + ADDRESS);
	}
}
