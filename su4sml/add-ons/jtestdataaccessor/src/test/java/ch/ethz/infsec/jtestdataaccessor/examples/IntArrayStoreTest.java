package ch.ethz.infsec.jtestdataaccessor.examples;

import java.io.FileNotFoundException;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.Ignore;

import antlr.RecognitionException;
import antlr.TokenStreamException;
import ch.ethz.infsec.jtestdataaccessor.TestDataAccessor;
import ch.ethz.infsec.jtestdataaccessor.TestDataParseException;
import ch.ethz.infsec.jtestdataaccessor.TestDataUser;
import ch.ethz.infsec.jtestdataaccessor.TestHelper;

public class IntArrayStoreTest extends AbstractIntArrayStoreTest implements
		TestDataUser {

	public static String classUnderTest = "ch.ethz.infsec.jtestdataaccessor.examples.IntArrayStore";

	private static TestDataAccessor tda;

	private static TestHelper th;

	public String getClassUnderTest() {
		return classUnderTest;
	}

	public Object getTestObject() {
		return testObject;
	}

	@BeforeClass
	public static void basicEnvSetup() throws FileNotFoundException,
			RecognitionException, TokenStreamException, TestDataParseException {
		tda = new TestDataAccessor(
				"src/test/resources/ch/ethz/infsec/jtestdataaccessor/examples/TestdataIntArrayStore");
		th = new TestHelper(classUnderTest, tda);
	}

	@Test
	public void getSortedTest() throws Throwable {
		String methodname = "getSorted";
		th.doTest(methodname, this);
	}

	public int[] wrapped_getSorted(){
		return testObject.getSorted();
	}
	
	@Ignore
	@Test
	public void getInvalidSortedTest() throws Throwable {
		String methodname = "getInvalidSorted";
		th.doTest(methodname, this);
	}
}
