package ch.ethz.infsec.jtestdataaccessor.examples;

import static org.junit.Assert.fail;

public abstract class AbstractIntArrayStoreTest {

	static IntArrayStore testObject;

	/**
	 * Setup the {@link IntArrayStore} with some test array.
	 * 
	 */
	public void setup1() {
		testObject = new IntArrayStore(new int[] { 9, 2, 4, 4, 2, 5, 76, 8, 6,
				4, 3, 2 });
	}

	/**
	 * Check if the given result is sorted in ascending order.
	 * 
	 * @param result
	 *            to check.
	 */
	public void resultCheck1(int[] result) {
		for (int i : result) {
			System.out.print(i + " ");
		}
		System.out.println();
		if (result.length > 1) {
			for (int i = 1; i < result.length; i++) {
				if (result[i - 1] > result[i]) {
					fail("Incorrectly sorted array.");
				}
			}
		}
	}

}
