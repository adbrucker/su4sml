package ch.ethz.infsec.jtestdataaccessor.nodes;

import java.io.FileNotFoundException;

import org.junit.BeforeClass;
import org.junit.Test;

import ch.ethz.infsec.jtestdataaccessor.TestDataAccessor;
import ch.ethz.infsec.jtestdataaccessor.TestDataParseException;
import ch.ethz.infsec.jtestdataaccessor.TestDataUser;
import ch.ethz.infsec.jtestdataaccessor.TestHelper;

import antlr.RecognitionException;
import antlr.TokenStreamException;

public class ValueTest extends AbstractValueTest implements TestDataUser {

	public static String classUnderTest = "ch.ethz.infsec.jtestdataaccessor.nodes.Value";

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
				"src/test/resources/ch/ethz/infsec/jtestdataaccessor/nodes/TestdataValue");
		th = new TestHelper(classUnderTest,tda);
	}
	
	@Test
	public void getValueTest() throws Throwable {
		String methodname = "getValue";
		th.doTest(methodname, this);
	}
	
	public Object wrapped_getValue() throws Throwable{
		return testObject.getValue();
	}
}
