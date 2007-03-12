package ch.ethz.infsec.jtestdataaccessor.nodes;

import ch.ethz.infsec.jtestdataaccessor.TestData;

/**
 * Represent a value which is found on the right hand side of the file with
 * testdata.
 * 
 * @author ms
 * 
 */
public abstract class Argument {

	protected TestData testdata;

	/**
	 * Convert to String, prepending some indentation.
	 * 
	 * @param indent
	 *            the required indentation.
	 * @return a pretty {@link String}.
	 */
	abstract public String toString(String indent);

	/**
	 * Set the {@link TestData} object this {@link Argument} belongs to.
	 */
	public void setTestData(TestData testdata) {
		this.testdata = testdata;
	}

}