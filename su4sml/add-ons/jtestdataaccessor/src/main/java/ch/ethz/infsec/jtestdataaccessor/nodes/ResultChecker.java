/*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * ResultChecker.java --- 
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.ComparisonFailure;

import ch.ethz.infsec.jtestdataaccessor.TestCase;
import ch.ethz.infsec.jtestdataaccessor.TestData;

/**
 * Class used for checking the result.
 * 
 * @author ms
 * 
 */
public class ResultChecker extends Argument {

	/**
	 * An enum of some default checkers which are mostly taken from
	 * org.junit.Assert.
	 * 
	 * @author ms
	 * 
	 */
	public static enum DefaultChecker {
		EQUALS, FALSE, TRUE, NOTNULL, NULL, NOTSAME, SAME, FAIL, NOTEQUAL
	};

	private DefaultChecker defaultchecker;

	private Function functionchecker;

	private TestCase testcase;

	/**
	 * Create new {@link ResultChecker} using a {@link DefaultChecker}.
	 * 
	 * @param defaultchecker
	 *            a {@link DefaultChecker}
	 */
	public ResultChecker(DefaultChecker defaultchecker) {
		this.defaultchecker = defaultchecker;
	}

	/**
	 * Create a new {@link ResultChecker} using a {@link Function}.
	 * 
	 * @param functionchecker
	 *            a {@link Function}
	 */
	public ResultChecker(Function functionchecker, TestData td) {
		functionchecker.setTestData(td);
		this.setTestData(td);
		this.functionchecker = functionchecker;
	}

	@Override
	public String toString() {
		return toString("");
	}

	/**
	 * Pretty print the {@link ResultChecker}.
	 * 
	 * @return the pretty printed {@link ResultChecker}
	 */
	@Override
	public String toString(String indent) {
		StringBuffer sb = new StringBuffer();
		if (defaultchecker != null) {
			sb.append(defaultchecker.toString());
		} else if (functionchecker != null) {
			sb.append(functionchecker.toString());
		}
		return indent + sb.toString();
	}

	/**
	 * Execute a check on a given object.
	 * 
	 * @param result
	 *            the actual result.
	 * @param reference
	 *            the reference value.
	 * @throws Throwable
	 */
	public void check(Object result) throws Throwable {
		Object expected = null;
		if (testcase.getResult() != null) {
			expected = testcase.getResult().getValue();
		}
		String comment = null;
		if (testcase != null) {
			comment = testcase.getComment();
		}
		if (defaultchecker != null) {
			switch (defaultchecker) {
			case EQUALS:
				assertEquals(comment, expected, result);
				break;
			case FAIL:
				fail(comment);
				break;
			case FALSE:
				assertFalse(comment, (Boolean) result);
				break;
			case NOTNULL:
				assertNotNull(comment, result);
				break;
			case NOTSAME:
				assertNotSame(comment, expected, result);
				break;
			case NULL:
				assertNull(comment, result);
				break;
			case SAME:
				assertSame(comment, expected, result);
				break;
			case TRUE:
				assertTrue(comment, (Boolean) result);
				break;
			case NOTEQUAL:
				assertNotEqual(comment, expected, result);
				break;
			}
		} else if (functionchecker != null) {
			Class argType = Object.class;
			if (testcase != null && testcase.getFunctionUnderTest() != null
					&& testcase.getFunctionUnderTest().getResultType() != null) {
				argType = testcase.getFunctionUnderTest().getResultType()
						.getTypeClass();
			}
			functionchecker.call(new Class[] { argType },
					new Object[] { result });
		} else {
			fail("No checker provided.");
		}
	}

	/**
	 * Asserts that two objects are not equal. If they are not, an
	 * {@link AssertionError} is thrown with the given message. Adapted from
	 * org.junit.Assert.
	 * 
	 * @param message
	 *            the identifying message or <code>null</code> for the
	 *            {@link AssertionError}
	 * @param expected
	 *            expected value
	 * @param actual
	 *            actual value
	 */
	static public void assertNotEqual(String message, Object unexpected,
			Object actual) {
		if (unexpected == null && actual == null) {
			return;
		}
		if (unexpected != null && !unexpected.equals(actual)) {
			return;
		}
		if (unexpected instanceof String && actual instanceof String) {
			throw new ComparisonFailure(message, (String) unexpected,
					(String) actual);
		} else {
			String formatted = "";
			if (message != null) {
				formatted = message + " ";
			}
			fail(formatted + "unexpected:<" + unexpected
					+ "> but nevertheless got:<" + actual + ">");
		}
	}

	/**
	 * Set the {@link TestCase} this {@link ResultChecker} belongs to.
	 * 
	 * @param testcase
	 *            the {@link TestCase} this object belongs to.
	 */
	public void setTestCase(TestCase testcase) {
		this.testcase = testcase;
	}

}
