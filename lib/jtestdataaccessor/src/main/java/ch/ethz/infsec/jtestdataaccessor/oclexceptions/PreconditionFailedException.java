package ch.ethz.infsec.jtestdataaccessor.oclexceptions;

public class PreconditionFailedException extends OclException {

	/**
	 * Thrown whenever a precondition check failed.
	 * 
	 * @param string
	 *            contains a message.
	 */
	public PreconditionFailedException(String string) {
		super(string);
	}

	/**
	 * A default serial version uid.
	 */
	private static final long serialVersionUID = 1L;

}
