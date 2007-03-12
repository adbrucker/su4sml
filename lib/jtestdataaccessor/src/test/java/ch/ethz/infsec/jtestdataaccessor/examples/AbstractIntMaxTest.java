package ch.ethz.infsec.jtestdataaccessor.examples;


public abstract class AbstractIntMaxTest {

	static IntMax testObject;

	/**
	 * Return an array as input for the test
	 * 
	 */
	public int[] getInput1() {
		return new int[] {2,5,4,3,5,6,7,5,3,2,4,6,8,9,0};
	}
	
	/**
	 * Return an array as input for the test, including negative values
	 * 
	 */
	public int[] getInput2() {
		return new int[] {1,0,-1};
	}

}
