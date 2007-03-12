header {
	package ch.ethz.infsec.jtestdataaccessor.parser;
	import ch.ethz.infsec.jtestdataaccessor.*;
	import ch.ethz.infsec.jtestdataaccessor.nodes.*;
	import java.util.*;
}

/**
 * Parser for test data.
 */
class TestdataParser extends Parser;

{
	/* Root of tree with tests */
	private TestData testdata;
	
	/* Name of the function whose entry is currently parsed */
	private String currentFunctionName;
	
	/* Function which is currently parsed */
	private FunctionUnderTest cfut;
	
	/* Current testcase */
	private TestCase ctest;
	
	/* Parser currently in the scope of a function? */
	boolean fscope = true;
	
	/**
	 * Set the root of the tree where the testdata shall be written to 
	 */
	public void setTestdata(TestData td){
			testdata = td;
	}
	
	/**
	 * Identifiers which are used in the file with testdata.
	 */
	private enum ident {
		RESULTTYPE, INPUTTYPES, SETUP, TEARDOWN, INPUT, RESULT, CHECKER, COMMENT
	};
}

/* Root of the parser */
startRule throws TestDataParseException:
	( sectionlist )
;

/* Section head with a name (which is a function) */
section returns [String value=""]:
		(LBRAK id:IDENTIFIER RBRAK)
		{
			value = id.getText();
		}
;

/* A list of sections which contain definitions or testcase specifications */
sectionlist throws TestDataParseException
	{
		String fname = null;
	}
	:
		( fname=section 
		{  
			/* Setup target of parsed data */
        	currentFunctionName = fname;
        	cfut = new FunctionUnderTest(currentFunctionName);
	        testdata.addTest(cfut);
		} 
		( definition | test )* ( sectionlist )?)
;

/* A definition, such as setup = ...; */
definition throws TestDataParseException
	{ 
		List<Argument> vl = null; 
		ident cident = null;
	}
	:
		( (cident=ident EQUALS vl=valuelist) )
		{
			if(cident != null && vl != null && vl.size() > 0){
				if(fscope){
					/* Current scope is a function */
					switch(cident){
						case RESULTTYPE:
							if(vl.size() == 1 && vl.get(0) instanceof Type){
								cfut.setResultType((Type)vl.get(0));
							}else{
								throw new TestDataParseException("resulttype only takes one type argument.", LT(0));
							}
							break;
						case INPUTTYPES:
							for(Argument arg: vl){
								if(arg instanceof Type){
									Type type = (Type) arg;
									cfut.addInputType(type);
								}else{
									throw new TestDataParseException("inputtypes only takes type arguments.",LT(0));
								}
							}
							break;
						case SETUP:
							if(vl.size() == 1 && vl.get(0) instanceof Function){
								cfut.setSetup((Function) vl.get(0));
							}else{
								throw new TestDataParseException("setup requires a function argument.",LT(0));
							}
							break;
						case TEARDOWN:
							if(vl.size() == 1 && vl.get(0) instanceof Function){
								cfut.setTeardown((Function) vl.get(0));
							}else{
								throw new TestDataParseException("teardown requires a function argument.",LT(0));
							}
							break;
						default:
							throw new TestDataParseException("Identifier "+cident+" not allowed in function scope.", LT(0));
					}
				}else{
					/* Current scope is a testcase */
					switch(cident){
						case INPUT:
							for(Argument arg: vl){
								if(arg instanceof Value){
									ctest.addInput((Value)arg);
								}else{
									throw new TestDataParseException("Inputs must be Values!", LT(0));
								}
							}
							break;
						case RESULT:
							if(vl.size() == 1 && vl.get(0) instanceof Value){
								ctest.setResult((Value)vl.get(0));
							}else if(vl.size() == 1 && vl.get(0) instanceof Type){
								ctest.setResult((Type)vl.get(0));
							}else{
									throw new TestDataParseException("Result must be a Value!",LT(0));
							}
							break;
						case CHECKER:
							if(vl.size() == 1){
								if(vl.get(0) instanceof Function){
									ctest.setChecker(new ResultChecker((Function)vl.get(0),testdata));
								}else if(vl.get(0) instanceof ResultChecker){
									ctest.setChecker((ResultChecker)vl.get(0));
								}
							}
							break;
						case COMMENT:
							if(vl.size() == 1 && vl.get(0) instanceof SimpleValue){
								ctest.setComment(((SimpleValue)vl.get(0)).toString());
							}else{
								throw new TestDataParseException("comment requires a string argument.",LT(0));
							}
							break;
						default:
							throw new TestDataParseException("Identifier "+cident+" not allowed in test scope.", LT(0));
					}
				}
			}
		}
;

/* An identifier/keyword such as setup */
ident returns [ident value=null]
	:
	(
		RESULTTYPE { value=ident.RESULTTYPE; } 
		| INPUTTYPE { value = ident.INPUTTYPES; } 
		| SETUP {	value = ident.SETUP; }
		| TEARDOWN { value = ident.TEARDOWN; }
		| INPUT { value = ident.INPUT; }
		| RESULT { value = ident.RESULT; }
		| CHECKER { value = ident.CHECKER; }
		| COMMENT { value = ident.COMMENT; }
	)
;

/* A testcase which is enclosed in {...} */
test throws TestDataParseException
	:
		( STEST {
			ctest = new TestCase();
			cfut.addTest(ctest);	
			/* Current scope is test */
			fscope = false; 
		} (definition)+ { fscope = true; } ETEST )
;

/* A list of values, eg. "foo",10,true,"bar" */
valuelist returns [Vector<Argument> value=null]
	{
		ResultChecker.DefaultChecker c = null;
	}
	:
		( (id:IDENTIFIER ((l:LPAREN r:RPAREN) | (lb:LBRAK rb:RBRAK))? | s:STRING | (neg:DASH)? (i:INT | f:FLOAT) | tr:TRUE | fa:FALSE | ch:CHAR | c=checker) ( ( COMMA value=valuelist ) | SEMICOLON ) )
		{
			if(value == null){
				value = new Vector<Argument>();
			}

			/* Numbers should be negated */
			String negate = "";
			if(neg != null){
					negate = "-";
			}

			Argument a = null;
			/* Create an object which represents the value */
			if(id != null){
				if(l != null && r != null){
					a = new Function(id.getText());				
				}else if(lb != null && rb != null){
					a = new Type(id.getText());
					((Type) a).setArray(true);
				}else{
					a = new Type(id.getText());
				}
			}else if(s != null){
				a = new SimpleValue(s.getText().replaceAll("\"",""));
			}else if(i != null){
				a = new SimpleValue(new Integer(negate+i.getText()));
			}else if(f != null){
				a = new SimpleValue(new Double(negate+f.getText())); 	
			}else if(tr != null){
				a = new SimpleValue(new Boolean(tr.getText()));
			}else if(fa != null){
				a = new SimpleValue(new Boolean(fa.getText()));
			}else if(ch != null){
				a = new SimpleValue(new Character(ch.getText().charAt(1)));
			}else if(c != null){
				a = new ResultChecker(c);
			}
			if(a != null){
				value.insertElementAt(a,0);
			}
		}
;

/* Checker which is used to check the result */
checker returns [ResultChecker.DefaultChecker value = null]
	: (e:EQUALS | nt:FALSEC | t:TRUEC | nn:NOTNULL | n:NULL | ns:NOTSAME | s:SAME | f:FAIL | ne:NOTEQUAL)
	{
		if(e != null){
			value = ResultChecker.DefaultChecker.EQUALS;
		}else if(nt != null){
			value = ResultChecker.DefaultChecker.FALSE;
		}else if(t != null){
			value = ResultChecker.DefaultChecker.TRUE;
		}else if(nn != null){
			value = ResultChecker.DefaultChecker.NOTNULL;
		}else if(n != null){
			value = ResultChecker.DefaultChecker.NULL;
		}else if(ns != null){
			value = ResultChecker.DefaultChecker.NOTSAME;
		}else if(s != null){
			value = ResultChecker.DefaultChecker.SAME;
		}else if(f != null){
			value = ResultChecker.DefaultChecker.FAIL;
		}else if(ne != null){
			value = ResultChecker.DefaultChecker.NOTEQUAL;
		}
	}
;


/** 
 * The Lexer for testdata
 */
class TestdataLexer extends Lexer;

/* Tokens which are keywords. The must not be used elsewhere in the file with testdata! */
tokens {
	RESULTTYPE = "resulttype";
	INPUTTYPE = "inputtypes";
	SETUP = "setup";
	TEARDOWN = "teardown";
	INPUT = "input";
	RESULT = "result";
	CHECKER = "checker";
	COMMENT = "comment";
	TRUE = "true";
	FALSE = "false";
	EQUALS = "EQUALS";
	FALSEC = "FALSE";
	TRUEC = "TRUE";
	NOTNULL = "NOTNULL";
	NULL = "NULL";
	NOTSAME = "NOTSAME";
	NOTEQUAL = "NOTEQUAL";
	SAME = "SAME";
	FAIL = "FAIL";
}

/* Alphanumeric identifiers */
IDENTIFIER: ( ( 'a'..'z'|'A'..'Z' ) ('a'..'z'|'A'..'Z'|'0'..'9'|'-'|'_'|'.')* );

/* Strings */
STRING: ('"' (~('"'))* '"' );

/* Single characters */
CHAR: ('\''.'\'');

/* Left ( */
LPAREN: ( '(' );

/* Right ) */
RPAREN: ( ')' );

/* Left [ */
LBRAK: ( '[' );

/* Right ] */
RBRAK: ( ']' );

/* Integers */
protected
INT :   ('0'..'9')+;

/* Floats */
protected
FLOAT:   INT '.' INT;

/* FLOAT or INT? */
RANGE_OR_INT
	:   ( INT '.' )  => FLOAT { $setType(FLOAT); }
	|   INT                  { $setType(INT); }
;

/* Start of a test */
STEST: ( '{' );

/* End of a test */
ETEST: ( '}' );

/* The equals sign */
EQUALS: '=';

/* A dash */
DASH: '-';

/* A comma */
COMMA: ',';

/* A semicolon */
SEMICOLON: ';';

/* Ignore newlines, whitespace and comments */
NEWLINE
    :   ('\r' '\n' { newline(); }  // DOS
    |   '\n'      { newline(); }  // UNIX
	|	' '
	|	'\t'
	|	'#' (~('\n' | '\r'))*
	)
	{ $setType(Token.SKIP); } //ignore these tokens
;
