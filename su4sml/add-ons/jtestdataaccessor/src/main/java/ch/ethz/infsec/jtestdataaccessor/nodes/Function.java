package ch.ethz.infsec.jtestdataaccessor.nodes;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import static org.junit.Assert.fail;

/**
 * A function node for a function which is used to set up the environment or to
 * create some value.
 * 
 * @author ms
 * 
 */
public class Function extends Value {

	private String name;

	/**
	 * Initialize a new function.
	 * 
	 * @param name
	 *            of the new function.
	 */
	public Function(String name) {
		this.name = name;
	}

	/**
	 * Get the name of the function.
	 * 
	 * @return the name.
	 */
	public String getName() {
		return name;
	}

	/**
	 * Call the function on the object with the junit tests, discarding the
	 * result.
	 * 
	 * @throws Throwable
	 * 
	 */
	public void call() throws Throwable {
		getValue();
	}

	/**
	 * Call the function using the given parameter types and arguments on the
	 * object with the junit tests, discarding the result.
	 * 
	 * @throws Throwable
	 * 
	 */
	public void call(Class[] classes, Object[] objects) throws Throwable {
		getValue(classes, objects);
	}

	/**
	 * Call the function on the object with the junit tests, but do not discard
	 * the result.
	 * 
	 * @throws Throwable
	 */
	@Override
	public Object getValue() throws Throwable {
		return getValue((Class[]) null, (Object[]) null);
	}

	/**
	 * Call the function using the given parameter types and arguments on the
	 * object with the junit tests and return the result.
	 * 
	 * @param params
	 *            types of the parameters.
	 * @param args
	 *            arguments to call the function with.
	 * @return the value produced by the called function.
	 * @throws Throwable
	 */
	public Object getValue(Class[] params, Object[] args) throws Throwable {
		try {
			if (testdata != null && testdata.getTestDataAccessor() != null
					&& testdata.getTestDataAccessor().getTestDataUser() != null) {
				Class c = testdata.getTestDataAccessor().getTestDataUser()
						.getClass();
				Method m = c.getMethod(name, params);
				return m.invoke(testdata.getTestDataAccessor()
						.getTestDataUser(), args);
			} else {
				System.err
						.println("No TestDataUser found to execute method on!");
			}
		} catch (InvocationTargetException e) {
			// The called function has thrown an exception, so return that
			// exception instead of a InvocationTargetException which is not
			// really useful outside.
			throw e.getCause();
		} catch (Exception e) {
			// There seems to be an exception in our code, which should not
			// happen...
			e.printStackTrace();
			fail("Failed to execute the specified function " + getName() + ".");
		}
		return null;
	}

	@Override
	public String toString() {
		return toString("");
	}

	/**
	 * Pretty print the function name.
	 * 
	 * @return the pretty name.
	 */
	@Override
	public String toString(String indent) {
		return indent + name + "()";
	}

}