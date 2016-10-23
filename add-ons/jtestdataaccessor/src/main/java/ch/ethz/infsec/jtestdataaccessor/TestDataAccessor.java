/*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * TestDataAccessor.java --- 
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007, ETH Zurich, Switzerland
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of the copyright holders nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************/
/* $Id$ */

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
