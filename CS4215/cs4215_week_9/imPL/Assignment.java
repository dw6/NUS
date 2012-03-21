package imPL;

public class Assignment implements Expression {
    public String leftHandSide;
    public Expression rightHandSide;
   public Assignment(String l, Expression r) {
      leftHandSide = l;
      rightHandSide = r;
   }

    // //////////////////////
    // Denotational Semantics
    // //////////////////////

    // stub to be replaced by proper implementation

    public StoreAndValue eval(Store s, Environment e) {
	return new StoreAndValue(s,new BoolValue(true));
    }

    // //////////////////////
    // Support Functions
    // //////////////////////

   public String toString() {
       return "(" + leftHandSide + " := " + rightHandSide + ")";
   }
}
