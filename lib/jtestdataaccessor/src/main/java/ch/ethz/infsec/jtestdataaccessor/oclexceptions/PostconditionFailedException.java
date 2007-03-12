package ch.ethz.infsec.jtestdataaccessor.oclexceptions;

public class PostconditionFailedException extends OclException {

	/**
	 * Thrown when a postcondition check failed.
	 * 
	 * @param string
	 *            contains a message.
	 */
	public PostconditionFailedException(String string) {
		super(string);
	}

	/**
	 * A default serial version uid.
	 */
	private static final long serialVersionUID = 1L;

}
