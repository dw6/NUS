package simPL;

public class Variable implements Expression {

   public String varname;

   public Variable(String nam) {
      varname = nam;
   }

    public Expression eliminateLet() {
	return this;
    }

    // //////////////////////
    // Static Semantics
    // //////////////////////

    public Type check(TypeEnvironment G) throws TypeError {
	Type varType = G.access(varname);
	if (varType!=null) return varType;
	else throw new TypeError("variable "+varname+" undefined");
    }

    // //////////////////////
    // Support Functions
    // //////////////////////

    public String toString() {
	return varname;
    }

    public String toXML() {
	return "<simpl:variable>" + varname + "</simpl:variable>\n";
    }
}

