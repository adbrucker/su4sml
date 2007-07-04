/*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * TestHelper.java --- 
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
