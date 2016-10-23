/*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * ValueTest.java --- 
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
