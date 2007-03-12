package ch.ethz.infsec.jtestdataaccessor;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;

import org.junit.Test;

import antlr.RecognitionException;
import antlr.TokenStreamException;

public class BasicTest {

	private void testConfigs(String path, boolean valid)
			throws FileNotFoundException, RecognitionException,
			TokenStreamException, TestDataParseException {
		File dir = new File(path);
		File[] files = dir.listFiles(new FilenameFilter() {
			public boolean accept(File dirname, String name) {
				if (name.endsWith(".conf")) {
					return true;
				} else {
					return false;
				}
			}
		});
		for (File file : files) {
			System.out.println("Trying to parse " + file.getAbsolutePath());
			try {
				TestDataAccessor tda = new TestDataAccessor(file
						.getAbsolutePath());
				//System.out.println(tda.toString());
			} catch (TestDataParseException e) {
				System.err.println(e.getClass().getName()+": "+e.getMessage());
				if (valid) {
					throw e;
				}
			}
		}
	}

	@Test
	public void validConfigs() throws FileNotFoundException,
			RecognitionException, TokenStreamException, TestDataParseException {
		System.out
				.println("Running tests on presumably correct testdata-files:");
		testConfigs("src/test/resources/valid", true);
	}

	@Test
	public void invalidConfigs() throws FileNotFoundException,
			RecognitionException, TokenStreamException {
		System.out
				.println("Running tests on presumably incorrect testdata-files, so expect exceptions:");
		try {
			testConfigs("src/test/resources/invalid", false);
		} catch (TestDataParseException e) {
			e.printStackTrace();
		}

	}

	public static void main(String[] args) {
		BasicTest b = new BasicTest();
		try {
			b.validConfigs();
			b.invalidConfigs();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
