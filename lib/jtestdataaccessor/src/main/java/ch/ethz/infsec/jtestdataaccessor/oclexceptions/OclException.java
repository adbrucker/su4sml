package ch.ethz.infsec.jtestdataaccessor.oclexceptions;

public class OclException extends Exception {

	/**
	 * Thrown if the check of an OCL contract failed.
	 * 
	 * @param string
	 *            contains a message.
	 */
	public OclException(String string) {
		super(string);
	}

	/**
	 * A default serial version uid.
	 */
	private static final long serialVersionUID = 1L;

}
