package rVML;

public class RTN extends INSTRUCTION {
  public RTN() {
     OPCODE = OPCODES.RTN;
  }
  public String toString() {
     return "RTN";
  }
  public String toXML() {
     return "<rvm:RTN/>";
  }
}
