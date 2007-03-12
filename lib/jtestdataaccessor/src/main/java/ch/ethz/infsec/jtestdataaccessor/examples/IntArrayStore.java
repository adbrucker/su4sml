package ch.ethz.infsec.jtestdataaccessor.examples;

public class IntArrayStore {

	private int[] array;

	public IntArrayStore(int[] array) {
		this.array = array;
	}

	/**
	 * Use bubblesort to sort an int[]. Taken from
	 * http://de.wikipedia.org/wiki/Bubblesort.
	 * 
	 * @param array
	 *            to sort
	 * @return the sorted array.
	 */
	public int[] getSorted() {
		boolean sorted = true;
		do {
			sorted = true;
			for (int i = 1; i < array.length; i++) {
				if (array[i - 1] > array[i]) {
					swap(i - 1, i);
					sorted = false;
				}
			}
		} while (!sorted);
		return array;
	}

	/**
	 * Use bubblesort to "wrongly" sort an int[].
	 * 
	 * @param array
	 *            to sort
	 * @return the sorted array.
	 * @throws Exception
	 */
	public int[] getInvalidSorted() throws Exception {
		boolean sorted = true;
		do {
			sorted = true;
			for (int i = 1; i < array.length; i++) {
				if (array[i - 1] < array[i]) {
					swap(i - 1, i);
					sorted = false;
				}
			}
		} while (!sorted);
		return array;
	}

	/**
	 * Swap two elements of the array.
	 * 
	 * @param i
	 *            the first element.
	 * @param j
	 *            the second element.
	 */
	private void swap(int i, int j) {
		final int tmp = array[i];
		array[i] = array[j];
		array[j] = tmp;
	}
}
