package eVM;

import eVML.*;

import java.util.*;

public class VM
{

	// run takes an instruction array and executes its
	// instructions from left to right until DONE is reached

	public static Value run(INSTRUCTION[] instructionArray)
	{

		// initialize registers

		int pc = 0;

		// store first and second operands
		int first, second;
		
		Stack<Value> os = new Stack<Value>();

		// loop

		loop: while (true)
		{
			INSTRUCTION i = instructionArray[pc];

			System.out.println("pc: "+pc+"; instruction: "+i);

			switch (i.OPCODE) {

			// pushing integer constants on the operand stack
			// in form of IntValue instances

			case OPCODES.LDCI:
				os.push(new IntValue(Integer.parseInt(((LDCI) i).VALUE)));
				pc++;
				break;

			// pushing boolean constants on the operand stack
			// in form of BoolValue instances

			case OPCODES.LDCB:
				os.push(new BoolValue(((LDCB) i).VALUE.equals("true")));
				pc++;
				break;

			// primitive operations: pop arguments from operand stack
			// and push result back on operand stack

			// use the non-shortcut version
			// of Java disjunction; otherwise
			// second argument will not be popped
			// in case of true
			case OPCODES.OR:
				os.push(new BoolValue(((BoolValue) os.pop()).value | ((BoolValue) os.pop()).value));
				pc++;
				break;

			// use the non-shortcut version
			// of Java conjunction; otherwise
			// second argument will not be popped
			// in case of false
			case OPCODES.AND:
				os.push(new BoolValue(((BoolValue) os.pop()).value & ((BoolValue) os.pop()).value));
				pc++;
				break;

			case OPCODES.NOT:
				os.push(new BoolValue(!((BoolValue) os.pop()).value));
				pc++;
				break;
				
			case OPCODES.LT:
				
				second =  ((IntValue) os.pop()).value;
				first = ((IntValue) os.pop()).value;
				
				os.push(new BoolValue( first < second ));
			
				pc++;
				break;
				
			case OPCODES.GT:
				
				second =  ((IntValue) os.pop()).value;
				first = ((IntValue) os.pop()).value;
				
				os.push(new BoolValue( first < second ));
				pc++;
				break;	

			case OPCODES.EQ:
				os.push(new BoolValue(
						((IntValue) os.pop()).value == ((IntValue) os.pop()).value));
				pc++;
				break;

			case OPCODES.PLUS:
				os.push(new IntValue(((IntValue) os.pop()).value
						+ ((IntValue) os.pop()).value));
				pc++;
				break;
				
			case OPCODES.MINUS:
								
				second =  ((IntValue) os.pop()).value;
				first = ((IntValue) os.pop()).value;
								
				os.push(new IntValue(first - second));
				pc++;
				break;
			
			case OPCODES.TIMES:
				os.push(new IntValue(((IntValue) os.pop()).value * ((IntValue) os.pop()).value));
				pc++;
				break;
				
		
			case OPCODES.DIV:
				
				second =  ((IntValue) os.pop()).value;
				first = ((IntValue) os.pop()).value;
				
				/* Handling Division by Zero */
				/*
				 * At any point of time division by zero is encountered,
				 * the entire result would be deemed erroneous. As such,
				 * the entire stack will be popped since no meaningful 
				 * computation can carry on. 
				 * 
				 * The result should be an error, implemented as an ErrorValue.
				 */
				
				// Division by zero error!
				if (second == 0) {
					// Pop the entire stack
					while(!os.isEmpty()) 
					{
						os.pop();
					}
					
					os.push(new ErrorValue());
					break loop;
				}
				else 
				{
					os.push(new IntValue(first / second));
					pc++;
				}
				
				break;
				
			// DONE simply breaks the loop. The result is now
			// on top of the operand stack

			case OPCODES.DONE:
				break loop;

			default:
				System.out.println(" unknown opcode: " + i.OPCODE);
				pc++;
			}
		}

		// return what is on top of the operand stack
		return os.pop();
	}
}
