/*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * IntMaxTest.java --- 
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
