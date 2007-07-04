/*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * TestCase.java --- 
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

import java.util.List;
import java.util.Vector;

import org.junit.Assert;
import static org.junit.Assert.fail;

import ch.ethz.infsec.jtestdataaccessor.nodes.Argument;
import ch.ethz.infsec.jtestdataaccessor.nodes.ResultChecker;
import ch.ethz.infsec.jtestdataaccessor.nodes.Type;
import ch.ethz.infsec.jtestdataaccessor.nodes.Value;

/**
 * A testcase of a function. Contains inputvalues, the expected result, a
 * comment and a checker for the result.
 * 
 * @author ms
 * 
 */
public class TestCase {

	private String comment;

	private List<Value> inputs;

	private Value result;

	private ResultChecker checker;

	private TestData testdata;

	private FunctionUnderTest functionUnderTest;

	public TestCase() {
		inputs = new Vector<Value>();
	}

	/**
	 * Get the checker of the result.
	 * 
	 * @return the checker.
	 */
	public Argument getChecker() {
		return checker;
	}

	/**
	 * Set the checker for the result.
	 * 
	 * @param checker
	 *            the new checker.
	 */
	public void setChecker(ResultChecker checker) {
		checker.setTestData(testdata);
		checker.setTestCase(this);
		this.checker = checker;
	}

	/**
	 * Get the provided comment.
	 * 
	 * @return the comment.
	 */
	public String getComment() {
		return comment;
	}

	/**
	 * Set a new comment.
	 * 
	 * @param comment
	 *            the new comment.
	 */
	public void setComment(String comment) {
		this.comment = comment;
	}

	/**
	 * Get the provided input values.
	 * 
	 * @return the input values.
	 */
	public List<Value> getInputs() {
		return inputs;
	}

	/**
	 * Get the arguments which will be used to call the function under test.
	 * They may either be basic values or be returned by a user defined
	 * function.
	 * 
	 * @return the arguments.
	 * @throws Throwable
	 */
	public Object[] getArguments() throws Throwable {
		Object[] args = new Object[getInputs().size()];
		int i = 0;
		for (Value input : getInputs()) {
			args[i] = input.getValue();
			i++;
		}
		return args;
	}

	/**
	 * Add another input value.
	 * 
	 * @param input
	 *            a new input value.
	 */
	public void addInput(Value input) {
		input.setTestData(testdata);
		this.inputs.add(input);
	}

	/**
	 * Return the result value.
	 * 
	 * @return the result value.
	 */
	public Value getResult() {
		return result;
	}

	/**
	 * Set a new result value.
	 * 
	 * @param result
	 *            a new result value.
	 */
	public void setResult(Value result) {
		result.setTestData(testdata);
		this.result = result;
	}

	/**
	 * Check the result of a test run.
	 * 
	 * @param actual
	 *            the actual result.
	 * @param resulttype
	 *            the type of the actual result.
	 * @throws Throwable
	 */
	public void checkResult(Object actual) throws Throwable {
		if (checker != null) {
			checker.check(actual);
		} else {
			if (getResult() instanceof Type) {
				fail("A " + getResult().getValue()
						+ " exception was expected, but the result " + actual
						+ " is currently being checked. "
						+ (comment != null ? "Comment: "+comment : ""));
			} else {
				fail("No checker for this function provided. Please set a checker for this testcase.");
			}
		}
	}

	@Override
	public String toString() {
		return toString("");
	}

	/**
	 * Pretty print a testcase.
	 * 
	 * @param indent
	 *            indentation to use.
	 * @return the pretty string.
	 */
	public String toString(String indent) {
		StringBuffer sb = new StringBuffer();
		sb.append(indent + "Test:\n");
		sb.append(indent + "  Comment: " + comment + "\n");
		sb.append(indent + "  Arguments:\n");
		for (Argument arg : inputs) {
			sb.append(indent + arg.toString("    ") + "\n");
		}
		if (result != null) {
			sb.append(indent + "  Result:\n");
			sb.append(indent + result.toString("    "));
			sb.append("\n");
		}
		if (checker != null) {
			sb.append(indent + "  Checker:\n");
			sb.append(indent + checker.toString("    "));
			sb.append("\n");
		}
		return sb.toString();
	}

	/**
	 * Set the {@link TestData} this object belongs to.
	 * 
	 * @param testdata
	 */
	public void setTestData(TestData testdata) {
		this.testdata = testdata;
	}

	/**
	 * Set the {@link FunctionUnderTest} this object belongs to.
	 * 
	 * @param functionUnderTest
	 *            this object belongs to.
	 */
	public void setFunctionUnderTest(FunctionUnderTest functionUnderTest) {
		this.functionUnderTest = functionUnderTest;
	}

	/**
	 * Get the {@link FunctionUnderTest} this object belongs to.
	 * 
	 * @return the {@link FunctionUnderTest} this object belongs to.
	 */
	public FunctionUnderTest getFunctionUnderTest() {
		return this.functionUnderTest;
	}

}
