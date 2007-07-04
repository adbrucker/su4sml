package ch.ethz.infsec.jtestdataaccessor;

/**
 * Exception to be thrown when the provided testdata is not valid or does not
 * contain enough information.
 * 
 * @author ms
 * 
 */
public class InvalidTestDataException extends Exception {
	/**
	 * A default serial version UID.
	 */
	private static final long serialVersionUID = 1L;

	public InvalidTestDataException(String msg) {
		super(msg);
	}
}
