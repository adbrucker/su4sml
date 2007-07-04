package ch.ethz.infsec.jtestdataaccessor.oclexceptions;

public class InvariantFailedException extends OclException {

	/**
	 * Thrown when the check of the invariant failed.
	 * 
	 * @param string
	 *            contains a message.
	 */
	public InvariantFailedException(String string) {
		super(string);
	}

	/**
	 * A default serial version uid.
	 */
	private static final long serialVersionUID = 1L;

}
