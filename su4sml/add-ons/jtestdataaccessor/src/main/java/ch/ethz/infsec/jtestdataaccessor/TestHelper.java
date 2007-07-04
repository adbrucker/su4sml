package ch.ethz.infsec.jtestdataaccessor;

import static org.junit.Assert.fail;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

import ch.ethz.infsec.jtestdataaccessor.nodes.Type;
import ch.ethz.infsec.jtestdataaccessor.oclexceptions.OclException;
import ch.ethz.infsec.jtestdataaccessor.oclexceptions.PreconditionFailedException;

/**
 * Provide some static helper functions which may be used in all tests.
 */
public class TestHelper {

	private String classUnderTest;

	private TestDataAccessor tda;

	private TestDataUser tduser;

	/**
	 * Create new {@link TestHelper} object.
	 * 
	 * @param classUnderTest
	 *            name of the class which is tested - must include the package
	 *            the class is in.
	 * @param tda
	 *            the {@link TestDataAccessor} which is used to access the
	 *            testdata.
	 */
	public TestHelper(String classUnderTest, TestDataAccessor tda) {
		this.classUnderTest = classUnderTest;
		this.tda = tda;
	}

	/**
	 * Invoke a method with the given parameters and arguments on the given
	 * object.
	 * 
	 * @param aMethod
	 *            name of the method to call.
	 * @param params
	 *            parameter types of the method to call.
	 * @param args
	 *            actual arguments for the call.
	 * @return the result of the call.
	 * @throws Throwable
	 *             is thrown if something went wrong.
	 */
	private Object invoke(String aMethod, Class[] params, Object[] args)
			throws Throwable {
		try {
			Class c = tduser.getClass();
			Method m = c.getDeclaredMethod("wrapped_" + aMethod, params);
			return m.invoke(tduser, args);
		} catch (InvocationTargetException e) {
			throw e.getCause();
		}
	}

	/**
	 * Execute tests of a given method.
	 * 
	 * @param methodname
	 *            of the method which is currently tested.
	 * @param tdu
	 *            the user of the testdata, ie. the JUnit test calling this
	 *            method.
	 * @throws Throwable
	 *             if something went wrong.
	 */
	public void doTest(String methodname, TestDataUser tdu) throws Throwable {
		tduser = tdu;
		tda.setTestDataUser(tduser);
		List<FunctionUnderTest> tests = tda.getTests(methodname);
		if (tests != null) {
			// Execute tests for each section of the given function from the
			// testdata file
			for (FunctionUnderTest test : tests) {
				Class[] params = test.getParams();
				try {
					for (TestCase ctest : test.getTests()) {
						Object[] args = ctest.getArguments();
						if (test.getSetup() != null) {
							test.getSetup().call();
						}
						Object result = null;
						try {
							result = invoke(methodname, params, args);
							ctest.checkResult(result);
						} catch (PreconditionFailedException pfe) {
							System.err.println(pfe.getMessage()
									+ " <<< Precondition FAILURE!");
						} catch (OclException oe) {
							String comment = ctest.getComment();
							fail((comment == null ? "" : comment + ": ")
									+ oe.getMessage());
						} catch (Exception e) {
							// If there was an exception, this might have been
							// intentionally - so check if the result was a type
							// and the exception an instance of this type.
							if (ctest.getResult() instanceof Type) {
								Type exceptionType = (Type) ctest.getResult();
								if (!exceptionType.getTypeClass().isInstance(e)) {
									throw e;
								}
							} else {
								throw e;
							}
						}
						if (test.getTeardown() != null) {
							test.getTeardown().call();
						}
					}
				} catch (Exception e) {
					e.printStackTrace();
					fail("Unexpected exception thrown: " + e.getMessage());
				}
			}
		} else {
			System.err.println("No tests for method " + methodname
					+ " provided.");
		}
	}
}
