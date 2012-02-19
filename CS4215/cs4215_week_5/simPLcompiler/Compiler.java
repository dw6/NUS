package simPLcompiler;

import simPL.*;
import sVML.*;

import java.util.*;

public class Compiler
{
	private Vector<INSTRUCTION> instructions;
	private Stack<ToCompile> toCompileStack;
	private IndexTable indexTable;
	private boolean toplevel = true;

	public static void displayInstructionArray(INSTRUCTION[] is)
	{
		int k = 0;
		for (INSTRUCTION i : is)
			System.err.println(k++ + " " + i);
	}

	private static INSTRUCTION instructionFromOp(String op)
	{
		if (op.equals("+"))
		{
			return new PLUS();
		}
		else if (op.equals("-"))
		{
			return new MINUS();
		}
		else if (op.equals("*"))
		{
			return new TIMES();
		}
		else if (op.equals("/"))
		{
			return new DIV();
		}
		else if (op.equals("\\"))
		{
			return new NOT();
		}
		else if (op.equals("&"))
		{
			return new AND();
		}
		else if (op.equals("|"))
		{
			return new OR();
		}
		else if (op.equals("<"))
		{
			return new LESS();
		}
		else if (op.equals(">"))
		{
			return new GREATER();
		}
		else
		/* (op.equals("=")) */{
			return new EQUAL();
		}
	}

	public Compiler(Expression exp)
	{		
		instructions = new Vector<INSTRUCTION>();
		toCompileStack = new Stack<ToCompile>();
		toplevel = true;
		toCompileStack.push(new ToCompile(new LDF(), new IndexTable(), exp.eliminateLet()));
	}

	// compile

	public INSTRUCTION[] compile()
	{
		compileAll();
		return (INSTRUCTION[]) (instructions.toArray(new INSTRUCTION[1]));
	}

	// compileAll

	public void compileAll()
	{		
		while (!toCompileStack.empty())
		{
			ToCompile toCompile = (ToCompile) toCompileStack.pop();
			LDF funInstruction = toCompile.funInstruction;
			funInstruction.ADDRESS = instructions.size();
			indexTable = toCompile.indexTable;
			
			comp(toCompile.body, true, toCompile.funVar);
			toplevel = false;
		}
	}

	// compAll

	protected void compAll(Vector<Expression> es)
	{
		int i = 0;
		int s = es.size();
		while (i < s)
		{
			comp(es.elementAt(i++), false);			
		}

	}

	// comp

	protected void comp(Expression exp, boolean insertReturn)
	{
		comp(exp, insertReturn, "");
	}

	protected void comp(Expression exp, boolean insertReturn, String funVar)
	{
		// Function Application!
		if (exp instanceof Application)
		{
			// Compile the operator, which should be a function.
			Expression op = ((Application) exp).operator;
			
			// Add the instructions "s"
			comp(op, false);
			
			// Compile the rest of the operands
			// Add the instructions "s_1 ... s_n"
			compAll(((Application) exp).operands);
			
			// Add instruction: CALL n, where "n" is the number of operands in the function appl. 
			instructions.add(new CALL(((Application) exp).operands.size()));
			if (insertReturn)
			{
				// if it the very the topmost level function, then insert a "DONE", 
				// otherwise we need to insert a "RTN" instruction instead.
				instructions.add(toplevel ? (INSTRUCTION) new DONE() : (INSTRUCTION) new RTN());				
			}
		}
		// If Expression
		else if (exp instanceof If)
		{
			// Compile s1, the condition
			comp(((If) exp).condition, false);
			
			// Add instruction: JOF
			JOF JOFinstruction = new JOF();
			instructions.add(JOFinstruction);
			
			// Compile s2, the expression if 'true' [ TODO: Why need funVar? ]
			comp(((If) exp).thenPart, insertReturn, funVar);
			
		
			GOTO GOTOinstruction = null;
			if (!insertReturn)
			{
				GOTOinstruction = new GOTO();
				instructions.add(GOTOinstruction);
			}
			
			
			JOFinstruction.ADDRESS = instructions.size();
			comp(((If) exp).elsePart, insertReturn, funVar);
			if (!insertReturn)
			{
				GOTOinstruction.ADDRESS = instructions.size();				
			}
		}
		else
		{
			// Variable
			if (exp instanceof Variable)
			{
				instructions.add(new LD(indexTable.access(((Variable) exp).varname)));
			}
			// RecFun
			else if (exp instanceof RecFun)
			{
				Vector<String> formals = ((RecFun) exp).formals;
				LDRF i = new LDRF(formals.size());
				instructions.add(i);
				Enumeration<String> e = formals.elements();
				IndexTable newIndexTable = (IndexTable) indexTable.clone();
				String fun = ((RecFun) exp).funVar;
				newIndexTable.extend(fun);
				while (e.hasMoreElements())
					newIndexTable.extend((String) e.nextElement());
				toCompileStack.push(new ToCompile(i, newIndexTable, ((RecFun) exp).body, fun));
			}
			// Fun
			else if (exp instanceof Fun)
			{
				// Compilation of Function
				
				// LDF
				Vector<String> formals = ((Fun) exp).formals;
				LDF i = new LDF(formals.size());
				instructions.add(i);
				
				
				Enumeration<String> e = formals.elements();
				IndexTable newIndexTable = (IndexTable) indexTable.clone();
				while (e.hasMoreElements())
					newIndexTable.extend((String) e.nextElement());
				toCompileStack.push(new ToCompile(i, newIndexTable, ((Fun) exp).body));
			}
			else if (exp instanceof UnaryPrimitiveApplication)
			{
				comp(((UnaryPrimitiveApplication) exp).argument, false);
				instructions.add(instructionFromOp(((UnaryPrimitiveApplication) exp).operator));
			}
			else if (exp instanceof BinaryPrimitiveApplication)
			{
				comp(((BinaryPrimitiveApplication) exp).argument1, false);
				comp(((BinaryPrimitiveApplication) exp).argument2, false);
				instructions.add(instructionFromOp(((BinaryPrimitiveApplication) exp).operator));
			}
			else if (exp instanceof BoolConstant)
			{
				instructions.add(new LDCB(((BoolConstant) exp).value));
			}
			else if (exp instanceof IntConstant)
			{
				instructions.add(new LDCI(((IntConstant) exp).value));
			}
			else
			// unknown expression
			{
				// skip
			}

			if (insertReturn)
				instructions.add(toplevel ? (INSTRUCTION) new DONE() : (INSTRUCTION) new RTN());
		}
	}
}
