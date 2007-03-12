package ch.ethz.infsec.jtestdataaccessor;

import antlr.Token;

/**
 * Thrown when an error occured during parsing of the testdatafile.
 * 
 * @author ms
 * 
 */
public class TestDataParseException extends Exception {

	/**
	 * A default serial version uid.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Create a new {@link TestDataParseException}; the message contains the
	 * current line where the error occurred.
	 * 
	 * @param msg
	 *            the user defined message.
	 * @param t
	 *            the {@link Token} containing the line number.
	 */
	public TestDataParseException(String msg, Token t) {
		super("line: " + t.getLine() + ": " + msg);
	}

}
