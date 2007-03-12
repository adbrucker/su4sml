package ch.ethz.infsec.jtestdataaccessor.examples;

import java.io.FileNotFoundException;

import org.junit.BeforeClass;
import org.junit.Test;

import antlr.RecognitionException;
import antlr.TokenStreamException;
import ch.ethz.infsec.jtestdataaccessor.TestDataAccessor;
import ch.ethz.infsec.jtestdataaccessor.TestDataParseException;
import ch.ethz.infsec.jtestdataaccessor.TestDataUser;
import ch.ethz.infsec.jtestdataaccessor.TestHelper;
import ch.ethz.infsec.jtestdataaccessor.oclexceptions.InvariantFailedException;
import ch.ethz.infsec.jtestdataaccessor.oclexceptions.PostconditionFailedException;
import ch.ethz.infsec.jtestdataaccessor.oclexceptions.PreconditionFailedException;

public class IntMaxTest extends AbstractIntMaxTest implements TestDataUser {

	public static final String classUnderTest = "ch.ethz.infsec.jtestdataaccessor.examples.IntMax";

	public String getClassUnderTest() {
		return classUnderTest;
	}

	private static TestDataAccessor tda;

	private static TestHelper th;

	@BeforeClass
	public static void basicEnvSetup() throws FileNotFoundException,
			RecognitionException, TokenStreamException, TestDataParseException {
		tda = new TestDataAccessor(
				"src/test/resources/ch/ethz/infsec/jtestdataaccessor/examples/TestdataIntMax");
		th = new TestHelper(classUnderTest, tda);
	}

	@Test
	public void maxTest() throws Throwable {
		String methodname = "max";
		th.doTest(methodname, this);
	}

	public int wrapped_max(int[] array) throws Throwable {
		int result;
		// Precondition check
		if(array.length == 0){
			throw new PreconditionFailedException("Array was empty.");
		}
		// invoke method
		result = testObject.max(array);
		// Postcondition check
		if(array.length == 0){
			throw new PostconditionFailedException("Array got emptied!");
		}
		// Invariant check
		checkInvariant();
		return result;
	}
	
	public Object getTestObject() {
		return testObject;
	}

	public void checkInvariant() throws InvariantFailedException {
		
	}
}
