package ch.ethz.infsec.jtestdataaccessor;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.List;
import antlr.RecognitionException;
import antlr.TokenStreamException;

import ch.ethz.infsec.jtestdataaccessor.parser.TestdataLexer;
import ch.ethz.infsec.jtestdataaccessor.parser.TestdataParser;

/**
 * Provide fairly simple access to testdata.
 * 
 * @author ms
 * 
 */
public class TestDataAccessor {
	private File testdatafile;

	private TestData testdata;

	private TestDataUser testdatauser;

	/**
	 * Parse a given testdatafile.
	 * 
	 * @param testdatafile
	 *            the file containing testdata.
	 * @throws FileNotFoundException
	 * @throws RecognitionException
	 * @throws TokenStreamException
	 * @throws TestDataParseException
	 */
	public TestDataAccessor(String testdatafile) throws FileNotFoundException,
			RecognitionException, TokenStreamException, TestDataParseException {
		setTestdatafile(testdatafile);
	}

	/**
	 * Return the absolute path to the file containing testdata.
	 * 
	 * @return the path to the file with the testdata.
	 */
	public String getTestdatafile() {
		return testdatafile.getAbsolutePath();
	}

	/**
	 * Set a new testdatafile. This results in parsing the file and creating a
	 * new internal tree with testdata.
	 * 
	 * @param testdatafile
	 *            the path to the file with testdata.
	 * @throws FileNotFoundException
	 * @throws RecognitionException
	 * @throws TokenStreamException
	 * @throws TestDataParseException
	 */
	public void setTestdatafile(String testdatafile)
			throws FileNotFoundException, RecognitionException,
			TokenStreamException, TestDataParseException {
		this.testdatafile = new File(testdatafile);
		TestdataLexer lexer = new TestdataLexer(new FileInputStream(
				this.testdatafile));
		TestdataParser parser = new TestdataParser(lexer);
		testdata = new TestData();
		testdata.setTestDataAccessor(this);
		parser.setTestdata(testdata);

		parser.startRule();
	}

	/**
	 * Get the current user of this {@link TestDataAccessor}.
	 * 
	 * @return the current user.
	 */
	public TestDataUser getTestDataUser() {
		return testdatauser;
	}

	/**
	 * Set a new user of this {@link TestDataAccessor}.
	 * 
	 * @param testdatauser
	 *            the new user.
	 */
	public void setTestDataUser(TestDataUser testdatauser) {
		this.testdatauser = testdatauser;
	}

	/**
	 * Get the tests of a certain function.
	 * 
	 * @param function
	 *            the name of the function.
	 * @return the tests.
	 */
	public List<FunctionUnderTest> getTests(String function) {
		return testdata.getTests(function);
	}

	/**
	 * Pretty print the testdata.
	 * 
	 * @return the pretty string.
	 */
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append("Testdata of file " + testdatafile.getAbsolutePath() + ":\n");
		sb.append(testdata.toString());
		return sb.toString();
	}

}
