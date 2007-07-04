package ch.ethz.infsec.jtestdataaccessor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

/**
 * Root of the "tree" which contains the tests.
 * 
 * @author ms
 * 
 */
public class TestData {

	private Map<String, List<FunctionUnderTest>> functions;

	private TestDataAccessor testdataaccessor;

	public TestData() {
		functions = new HashMap<String, List<FunctionUnderTest>>();
	}

	/**
	 * Add testdata for another function to the testsuite.
	 * 
	 * @param test
	 *            testdata for the function.
	 */
	public void addTest(FunctionUnderTest test) {
		List<FunctionUnderTest> testlist = functions.get(test.getName());
		test.setTestData(this);
		if (testlist != null) {
			testlist.add(test);
		} else {
			testlist = new Vector<FunctionUnderTest>();
			testlist.add(test);
			functions.put(test.getName(), testlist);
		}
	}

	/**
	 * Get the tests of a given function.
	 * 
	 * @param function
	 *            the function.
	 * @return the tests.
	 */
	protected List<FunctionUnderTest> getTests(String function) {
		return functions.get(function);
	}

	/**
	 * Pretty print the testdata.
	 */
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		for (String fname : functions.keySet()) {
			sb.append("Function under test: " + fname + "\n");
			for (FunctionUnderTest function : functions.get(fname)) {
				sb.append(function.toString("  "));
			}
		}
		return sb.toString();
	}

	/**
	 * Set the {@link TestDataAccessor} these testdata belong to.
	 * 
	 * @param testdataaccessor
	 *            the testdataaccessor.
	 */
	public void setTestDataAccessor(TestDataAccessor testdataaccessor) {
		this.testdataaccessor = testdataaccessor;
	}

	/**
	 * Get the {@link TestDataAccessor} these testdata belong to.
	 * 
	 * @return the testdataaccessor.
	 */
	public TestDataAccessor getTestDataAccessor() {
		return testdataaccessor;
	}

}