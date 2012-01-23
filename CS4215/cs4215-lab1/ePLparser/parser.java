//----------------------------------------------------
// The following code was generated by CUP v0.11a beta 20060608
// Tue Jan 18 00:54:13 SGT 2011
//----------------------------------------------------

package ePLparser;

import ePL.*;
import java_cup.runtime.*;
import java.util.*;

/**
 * CUP v0.11a beta 20060608 generated parser.
 * 
 * @version Tue Jan 18 00:54:13 SGT 2011
 */
public class parser extends java_cup.runtime.lr_parser
{

	/** Default constructor. */
	public parser()
	{
		super();
	}

	/** Constructor which sets the default scanner. */
	public parser(java_cup.runtime.Scanner s)
	{
		super(s);
	}

	/** Constructor which sets the default scanner. */
	public parser(java_cup.runtime.Scanner s, java_cup.runtime.SymbolFactory sf)
	{
		super(s, sf);
	}

	/** Production table. */
	protected static final short _production_table[][] = unpackFromStrings(new String[] { "\000\017\000\002\002\003\000\002\002\004\000\002\002"
			+ "\003\000\002\002\003\000\002\002\005\000\002\002\005"
			+ "\000\002\002\005\000\002\002\005\000\002\002\005\000"
			+ "\002\002\005\000\002\002\004\000\002\002\005\000\002"
			+ "\002\005\000\002\002\005\000\002\002\005" });

	/** Access to production table. */
	public short[][] production_table()
	{
		return _production_table;
	}

	/** Parse-action table. */
	protected static final short[][] _action_table = unpackFromStrings(new String[] { "\000\035\000\014\004\004\005\005\006\006\007\010\022"
			+ "\007\001\002\000\030\002\001\010\001\011\001\012\001"
			+ "\013\001\014\001\015\001\016\001\017\001\020\001\021"
			+ "\001\001\002\000\030\002\uffff\010\uffff\011\uffff\012\uffff"
			+ "\013\uffff\014\uffff\015\uffff\016\uffff\017\uffff\020\uffff\021"
			+ "\uffff\001\002\000\030\002\ufffe\010\ufffe\011\ufffe\012\ufffe"
			+ "\013\ufffe\014\ufffe\015\ufffe\016\ufffe\017\ufffe\020\ufffe\021"
			+ "\ufffe\001\002\000\014\004\004\005\005\006\006\007\010"
			+ "\022\007\001\002\000\014\004\004\005\005\006\006\007"
			+ "\010\022\007\001\002\000\026\002\016\011\015\012\021"
			+ "\013\014\014\017\015\012\016\022\017\020\020\013\021"
			+ "\023\001\002\000\014\004\004\005\005\006\006\007\010"
			+ "\022\007\001\002\000\014\004\004\005\005\006\006\007"
			+ "\010\022\007\001\002\000\014\004\004\005\005\006\006"
			+ "\007\010\022\007\001\002\000\014\004\004\005\005\006"
			+ "\006\007\010\022\007\001\002\000\004\002\000\001\002"
			+ "\000\014\004\004\005\005\006\006\007\010\022\007\001"
			+ "\002\000\014\004\004\005\005\006\006\007\010\022\007"
			+ "\001\002\000\014\004\004\005\005\006\006\007\010\022"
			+ "\007\001\002\000\014\004\004\005\005\006\006\007\010"
			+ "\022\007\001\002\000\014\004\004\005\005\006\006\007"
			+ "\010\022\007\001\002\000\030\002\ufff8\010\ufff8\011\015"
			+ "\012\021\013\014\014\017\015\012\016\022\017\020\020"
			+ "\013\021\ufff8\001\002\000\030\002\ufffb\010\ufffb\011\ufffb"
			+ "\012\ufffb\013\ufffb\014\ufffb\015\ufffb\016\ufffb\017\ufffb\020"
			+ "\ufffb\021\ufffb\001\002\000\022\002\ufff5\010\ufff5\014\017"
			+ "\015\012\016\022\017\020\020\ufff5\021\ufff5\001\002\000"
			+ "\030\002\ufffa\010\ufffa\011\ufffa\012\ufffa\013\ufffa\014\ufffa"
			+ "\015\ufffa\016\ufffa\017\ufffa\020\ufffa\021\ufffa\001\002\000"
			+ "\030\002\ufffd\010\ufffd\011\ufffd\012\ufffd\013\ufffd\014\ufffd"
			+ "\015\ufffd\016\022\017\020\020\ufffd\021\ufffd\001\002\000"
			+ "\022\002\ufff4\010\ufff4\014\017\015\012\016\022\017\020"
			+ "\020\ufff4\021\ufff4\001\002\000\022\002\ufff6\010\ufff6\014"
			+ "\017\015\012\016\022\017\020\020\ufff6\021\ufff6\001\002"
			+ "\000\030\002\ufff9\010\ufff9\011\015\012\021\013\014\014"
			+ "\017\015\012\016\022\017\020\020\ufff9\021\ufff9\001\002"
			+ "\000\030\002\ufffc\010\ufffc\011\ufffc\012\ufffc\013\ufffc\014"
			+ "\ufffc\015\ufffc\016\022\017\020\020\ufffc\021\ufffc\001\002"
			+ "\000\026\010\036\011\015\012\021\013\014\014\017\015"
			+ "\012\016\022\017\020\020\013\021\023\001\002\000\030"
			+ "\002\ufff3\010\ufff3\011\ufff3\012\ufff3\013\ufff3\014\ufff3\015"
			+ "\ufff3\016\ufff3\017\ufff3\020\ufff3\021\ufff3\001\002\000\030"
			+ "\002\ufff7\010\ufff7\011\015\012\021\013\014\014\017\015"
			+ "\012\016\022\017\020\020\ufff7\021\ufff7\001\002" });

	/** Access to parse-action table. */
	public short[][] action_table()
	{
		return _action_table;
	}

	/** <code>reduce_goto</code> table. */
	protected static final short[][] _reduce_table = unpackFromStrings(new String[] { "\000\035\000\004\002\010\001\001\000\002\001\001\000"
			+ "\002\001\001\000\002\001\001\000\004\002\036\001\001"
			+ "\000\004\002\034\001\001\000\002\001\001\000\004\002"
			+ "\033\001\001\000\004\002\032\001\001\000\004\002\031"
			+ "\001\001\000\004\002\030\001\001\000\002\001\001\000"
			+ "\004\002\027\001\001\000\004\002\026\001\001\000\004"
			+ "\002\025\001\001\000\004\002\024\001\001\000\004\002"
			+ "\023\001\001\000\002\001\001\000\002\001\001\000\002"
			+ "\001\001\000\002\001\001\000\002\001\001\000\002\001"
			+ "\001\000\002\001\001\000\002\001\001\000\002\001\001"
			+ "\000\002\001\001\000\002\001\001\000\002\001\001" });

	/** Access to <code>reduce_goto</code> table. */
	public short[][] reduce_table()
	{
		return _reduce_table;
	}

	/** Instance of action encapsulation class. */
	protected CUP$parser$actions action_obj;

	/** Action encapsulation object initializer. */
	protected void init_actions()
	{
		action_obj = new CUP$parser$actions(this);
	}

	/** Invoke a user supplied parse action. */
	public java_cup.runtime.Symbol do_action(int act_num,
			java_cup.runtime.lr_parser parser, java.util.Stack stack, int top)
			throws java.lang.Exception
	{
		/* call code in generated class */
		return action_obj.CUP$parser$do_action(act_num, parser, stack, top);
	}

	/** Indicates start state. */
	public int start_state()
	{
		return 0;
	}

	/** Indicates start production. */
	public int start_production()
	{
		return 1;
	}

	/** <code>EOF</code> Symbol index. */
	public int EOF_sym()
	{
		return 0;
	}

	/** <code>error</code> Symbol index. */
	public int error_sym()
	{
		return 1;
	}

}

/** Cup generated class to encapsulate user supplied action code. */
class CUP$parser$actions
{
	private final parser parser;

	/** Constructor */
	CUP$parser$actions(parser parser)
	{
		this.parser = parser;
	}

	/** Method with the actual generated action code. */
	public final java_cup.runtime.Symbol CUP$parser$do_action(
			int CUP$parser$act_num,
			java_cup.runtime.lr_parser CUP$parser$parser,
			java.util.Stack CUP$parser$stack, int CUP$parser$top)
			throws java.lang.Exception
	{
		/* Symbol object for return from actions */
		java_cup.runtime.Symbol CUP$parser$result;

		/* select the action based on the action number */
		switch (CUP$parser$act_num) {
		/* . . . . . . . . . . . . . . . . . . . . */
		case 14: // expression ::= LPAREN expression RPAREN
		{
			Expression RESULT = null;
			int eleft = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 1)).left;
			int eright = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 1)).right;
			Expression e = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 1)).value;
			RESULT = e;
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack
									.elementAt(CUP$parser$top - 2)),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 13: // expression ::= expression EQUAL expression
		{
			Expression RESULT = null;
			int e1left = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).left;
			int e1right = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).right;
			Expression e1 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).value;
			int e2left = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).left;
			int e2right = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).right;
			Expression e2 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.peek()).value;
			RESULT = new BinaryPrimitiveApplication("=", e1, e2);
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack
									.elementAt(CUP$parser$top - 2)),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 12: // expression ::= expression LESS expression
		{
			Expression RESULT = null;
			int e1left = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).left;
			int e1right = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).right;
			Expression e1 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).value;
			int e2left = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).left;
			int e2right = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).right;
			Expression e2 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.peek()).value;
			RESULT = new BinaryPrimitiveApplication("<", e1, e2);
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack
									.elementAt(CUP$parser$top - 2)),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 11: // expression ::= expression GREATER expression
		{
			Expression RESULT = null;
			int e1left = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).left;
			int e1right = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).right;
			Expression e1 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).value;
			int e2left = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).left;
			int e2right = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).right;
			Expression e2 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.peek()).value;
			RESULT = new BinaryPrimitiveApplication(">", e1, e2);
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack
									.elementAt(CUP$parser$top - 2)),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 10: // expression ::= NEG expression
		{
			Expression RESULT = null;
			int eleft = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).left;
			int eright = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).right;
			Expression e = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.peek()).value;
			RESULT = new UnaryPrimitiveApplication("\\", e);
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack
									.elementAt(CUP$parser$top - 1)),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 9: // expression ::= expression OR expression
		{
			Expression RESULT = null;
			int e1left = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).left;
			int e1right = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).right;
			Expression e1 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).value;
			int e2left = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).left;
			int e2right = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).right;
			Expression e2 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.peek()).value;
			RESULT = new BinaryPrimitiveApplication("|", e1, e2);
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack
									.elementAt(CUP$parser$top - 2)),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 8: // expression ::= expression AND expression
		{
			Expression RESULT = null;
			int e1left = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).left;
			int e1right = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).right;
			Expression e1 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).value;
			int e2left = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).left;
			int e2right = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).right;
			Expression e2 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.peek()).value;
			RESULT = new BinaryPrimitiveApplication("&", e1, e2);
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack
									.elementAt(CUP$parser$top - 2)),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 7: // expression ::= expression DIV expression
		{
			Expression RESULT = null;
			int e1left = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).left;
			int e1right = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).right;
			Expression e1 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).value;
			int e2left = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).left;
			int e2right = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).right;
			Expression e2 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.peek()).value;
			RESULT = new BinaryPrimitiveApplication("/", e1, e2);
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack
									.elementAt(CUP$parser$top - 2)),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 6: // expression ::= expression TIMES expression
		{
			Expression RESULT = null;
			int e1left = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).left;
			int e1right = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).right;
			Expression e1 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).value;
			int e2left = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).left;
			int e2right = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).right;
			Expression e2 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.peek()).value;
			RESULT = new BinaryPrimitiveApplication("*", e1, e2);
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack
									.elementAt(CUP$parser$top - 2)),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 5: // expression ::= expression MINUS expression
		{
			Expression RESULT = null;
			int e1left = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).left;
			int e1right = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).right;
			Expression e1 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).value;
			int e2left = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).left;
			int e2right = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).right;
			Expression e2 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.peek()).value;
			RESULT = new BinaryPrimitiveApplication("-", e1, e2);
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack
									.elementAt(CUP$parser$top - 2)),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 4: // expression ::= expression PLUS expression
		{
			Expression RESULT = null;
			int e1left = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).left;
			int e1right = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).right;
			Expression e1 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 2)).value;
			int e2left = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).left;
			int e2right = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).right;
			Expression e2 = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.peek()).value;
			RESULT = new BinaryPrimitiveApplication("+", e1, e2);
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack
									.elementAt(CUP$parser$top - 2)),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 3: // expression ::= FALSE
		{
			Expression RESULT = null;
			RESULT = new BoolConstant("false");
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 2: // expression ::= TRUE
		{
			Expression RESULT = null;
			RESULT = new BoolConstant("true");
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 1: // $START ::= expression EOF
		{
			Object RESULT = null;
			int start_valleft = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 1)).left;
			int start_valright = ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 1)).right;
			Expression start_val = (Expression) ((java_cup.runtime.Symbol) CUP$parser$stack
					.elementAt(CUP$parser$top - 1)).value;
			RESULT = start_val;
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"$START",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack
									.elementAt(CUP$parser$top - 1)),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			/* ACCEPT */
			CUP$parser$parser.done_parsing();
			return CUP$parser$result;

			/* . . . . . . . . . . . . . . . . . . . . */
		case 0: // expression ::= INTEGER
		{
			Expression RESULT = null;
			int xleft = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).left;
			int xright = ((java_cup.runtime.Symbol) CUP$parser$stack.peek()).right;
			String x = (String) ((java_cup.runtime.Symbol) CUP$parser$stack
					.peek()).value;
			RESULT = new IntConstant(x);
			CUP$parser$result = parser
					.getSymbolFactory()
					.newSymbol(
							"expression",
							0,
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							((java_cup.runtime.Symbol) CUP$parser$stack.peek()),
							RESULT);
		}
			return CUP$parser$result;

			/* . . . . . . */
		default:
			throw new Exception(
					"Invalid action number found in internal parse table");

		}
	}
}
