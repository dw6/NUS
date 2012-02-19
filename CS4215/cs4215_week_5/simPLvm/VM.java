package simPLvm;

import sVML.*;

import java.awt.SystemColor;
import java.util.*;

public class VM
{
	public static Value run(INSTRUCTION[] instructionArray)
	{		
		int counter = 0;
		int pc = 0;
		/**
		 * This is the Operand Stack
		 */
		Stack<Value> os = new Stack<Value>();
		/**
		 * Run Time Stack
		 */
		Stack<StackFrame> rts = new Stack<StackFrame>();
		Environment e = new Environment();

		loop: while (true)
		{
			INSTRUCTION i = instructionArray[pc];
		

			System.out.println("pc: " + pc + "; instruction: " + i);
			System.err.println("");
			System.err.println("========== Environment =========== [ " + counter++ + " ]");
			System.err.println(e.toString());
			
			System.err.println("========== Operand Stack =========");
			for(Value i1: os) 
			{
				System.err.println("[ " + i1 + " ]");
			}
			
			System.err.println("========== Runtime Stack =========");
			for(StackFrame s : rts) 
			{
				System.err.println("[ " + s + " ]");
			}

			System.err.println("-----------------------------------------");
			switch (i.OPCODE) {
				
				case OPCODES.LDCI:
				{
					// Retrieve the value stored in the LDCI instruction. 
					os.push(new IntValue(((LDCI) i).VALUE));
					pc++;
					break;
				}
	
				case OPCODES.LDCB:
				{
					// Retrieve the value stored in the LDCB instruction. 
					os.push(new BoolValue(((LDCB) i).VALUE));
					pc++;
					break;
				}
	
				// primitive operations:
				// watch out, the non-commutative operations have to consider
				// that the arguments appear on the stack in reverse order!
	
				case OPCODES.PLUS:
				{
					os.push(new IntValue(((IntValue) os.pop()).value + ((IntValue) os.pop()).value));
					pc++;
					break;
				}
	
				case OPCODES.TIMES:
				{
					os.push(new IntValue(((IntValue) os.pop()).value * ((IntValue) os.pop()).value));
					pc++;
					break;
				}
	
				case OPCODES.MINUS:
				{
					os.push(new IntValue(-((IntValue) os.pop()).value + ((IntValue) os.pop()).value));
					pc++;
					break;
				}
	
				case OPCODES.OR:
				{
					boolean b1, b2;
					b2 = ((BoolValue) os.pop()).value;
					b1 = ((BoolValue) os.pop()).value;
					os.push(new BoolValue(b1 || b2));
					pc++;
					break;
				}
	
				case OPCODES.AND:
				{
					boolean b1, b2;
					b2 = ((BoolValue) os.pop()).value;
					b1 = ((BoolValue) os.pop()).value;
					os.push(new BoolValue(b1 && b2));
					pc++;
					break;
				}
	
				case OPCODES.NOT:
				{
					os.push(new BoolValue(!((BoolValue) os.pop()).value));
					pc++;
					break;
				}
	
				case OPCODES.GREATER:
				{
					os.push(new BoolValue(((IntValue) os.pop()).value < ((IntValue) os.pop()).value));
					pc++;
					break;
				}
	
				case OPCODES.LESS:
				{
					os.push(new BoolValue(((IntValue) os.pop()).value > ((IntValue) os.pop()).value));
					pc++;
					break;
				}
	
				case OPCODES.EQUAL:
				{
					os.push(new BoolValue(((IntValue) os.pop()).value == ((IntValue) os.pop()).value));
					pc++;
					break;
				}
	
				case OPCODES.DIV:
				{
					IntValue divisor = (IntValue) os.pop();
					if (divisor.value == 0)
					{
						os.push(new ErrorValue());
						break loop;
					}
					else
					{
						os.push(new IntValue((((IntValue) os.pop()).value / divisor.value)));
						pc++;
						break;
					}
				}
	
				case OPCODES.GOTO:
				{
					pc = ((GOTO) i).ADDRESS;
					break;
				}
	
				case OPCODES.JOF:
				{
					/**
					 * Jump if not equal.
					 */
					pc = (((BoolValue) os.pop()).value) ? pc + 1 : ((JOF) i).ADDRESS;
					break;
				}
	
				case OPCODES.LD:
				{
					/**
					 * Execution of identifiers
					 */
					os.push(e.elementAt(((LD) i).INDEX));
					System.err.println(e.toString());
					pc++;
					break;
				}
	
				case OPCODES.LDF:
				{
					Environment env = e;
					os.push(new Closure(env, ((LDF) i).ADDRESS));
					System.err.println("[ Adding Closure ] " + os.peek().toString());
					pc++;
					break;
				}
	
				case OPCODES.CALL:
				{
					// CALL extends the Environment!
					
					int n = ((CALL) i).NUMBEROFARGUMENTS;
					
					// os.size() - n - 1 is where the closure is located
					Closure closure = (Closure) os.elementAt(os.size() - n - 1);					
					Environment newEnv = closure.environment.extend(n);
					System.err.println("[ Closure ] " + closure.toString());

					int s = newEnv.size();
					for (int j = s - 1; j >= s - n; --j)
					{
						newEnv.setElementAt(os.pop(), j);
						System.err.println("Adding to env: " + newEnv.get(j));
					}
					os.pop(); // function value
					
					rts.push(new StackFrame(pc + 1, e, os));
					pc = closure.ADDRESS;
					e = newEnv;
					os = new Stack();
					break;
				}
				
				case OPCODES.RTN:
				{
					Value returnValue = os.pop();
					System.err.println("Popping stack frame: " + rts.peek().toString());
					StackFrame f = rts.pop();
					pc = f.pc;
					e = f.environment;
					os = f.operandStack;
					os.push(returnValue);
					System.err.println("Restored prev stack frame");
					break;
				}
				
				// DONE simply breaks the loop. The result is now
				// on top of the operand stack
				
				// [OK]
				case OPCODES.DONE:
				{
					break loop;
				}
	
				default:
				{
					System.out.println(" unknown opcode: " + i.OPCODE);
					pc++;
				}
			}
		}
		
		// This is the very last instruction.
		return (Value) os.pop();
	}
}
