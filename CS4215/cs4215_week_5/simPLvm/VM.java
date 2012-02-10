package simPLvm; 

import sVML.*;

import java.util.*;

public class VM {

	// run

	public static Value run(INSTRUCTION[] instructionArray) {

		// initialize registers

		int pc = 0;
		Stack<Value> os = new Stack<Value>();
		Stack<StackFrame> rts = new Stack<StackFrame>();
		Environment e = new Environment();

		// loop

		loop:
			while (true) {
				INSTRUCTION i = instructionArray[pc];

				// System.out.println("pc: "+pc+"; instruction: "+i);

				switch (i.OPCODE) {

				// pushing constants

				case OPCODES.LDCI: {
					os.push(new IntValue(((LDCI)i).VALUE));
					pc++;
					break;
				}

				case OPCODES.LDCB: {
					os.push(new BoolValue(((LDCB)i).VALUE));
					pc++;
					break;
				}

				// primitive operations:
				// watch out, the non-commutative operations have to consider
				// that the arguments appear on the stack in reverse order!

				case OPCODES.PLUS: {
					os.push(new IntValue(
							((IntValue) os.pop()).value
							+ 
							((IntValue) os.pop()).value));
					pc++;
					break;
				}

				case OPCODES.TIMES: {
					os.push(new IntValue(
							((IntValue) os.pop()).value
							* 
							((IntValue) os.pop()).value));
					pc++;
					break;
				}

				case OPCODES.MINUS: {
					os.push(new IntValue(
							- ((IntValue) os.pop()).value
							+
							((IntValue) os.pop()).value));
					pc++;
					break;
				}

				case OPCODES.OR: { 
					boolean b1, b2;
					b2 = ((BoolValue)os.pop()).value;
					b1 = ((BoolValue)os.pop()).value;
					os.push(new BoolValue(b1 || b2));
					pc++;
					break;
				}

				case OPCODES.AND:   { 
					boolean b1, b2;
					b2 = ((BoolValue)os.pop()).value;
					b1 = ((BoolValue)os.pop()).value;
					os.push(new BoolValue(b1 && b2));
					pc++;
					break;
				}

				case OPCODES.NOT: {
					os.push(new BoolValue(
							! ((BoolValue) os.pop()).value));
					pc++;
					break;
				}

				case OPCODES.GREATER: {
					os.push(new BoolValue(
							((IntValue) os.pop()).value
							<
							((IntValue) os.pop()).value));
					pc++;
					break;
				}

				case OPCODES.LESS: {
					os.push(new BoolValue(
							((IntValue) os.pop()).value
							>
							((IntValue) os.pop()).value));
					pc++;
					break;
				}

				case OPCODES.EQUAL: {
					os.push(new BoolValue(
							((IntValue) os.pop()).value
							==
								((IntValue) os.pop()).value));
					pc++;
					break;
				}

				// DONE simply breaks the loop. The result is now
				// on top of the operand stack

				case OPCODES.DONE: {  
					break loop;
				}

				default: {             
					System.out.println(" unknown opcode: " 
							+ i.OPCODE);
					pc++;
				}
				}
			}
		return (Value) os.pop();
	}
}
