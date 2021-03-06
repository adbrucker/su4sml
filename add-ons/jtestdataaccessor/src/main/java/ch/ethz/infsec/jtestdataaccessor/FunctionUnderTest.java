/*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * FunctionUnderTest.java --- 
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

import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import ch.ethz.infsec.jtestdataaccessor.nodes.Function;
import ch.ethz.infsec.jtestdataaccessor.nodes.Type;

/**
 * Contains the testcases, setup/teardown methods and types for a method which
 * is to be tested. There will be several instances of this class if there is
 * more than one section for the function in the file containing the testdata
 * configuration.
 * 
 * @author ms
 * 
 */
public class FunctionUnderTest {

	private Type resultType;

	private List<Type> inputTypes;

	private final String name;

	private List<TestCase> testCases;

	private Function teardown;

	private Function setup;

	private TestData testdata;

	/**
	 * Initialize the class.
	 * 
	 * @param name
	 *            of the function under test.
	 */
	public FunctionUnderTest(String name) {
		this.name = name;
		inputTypes = new Vector<Type>();
		testCases = new Vector<TestCase>();
		resultType = new Type("void");
	}

	/**
	 * Get a list with the input types. This will be used to select the correct
	 * method, especially in case of overloaded methods.
	 * 
	 * @return the list of input types.
	 */
	public List<Type> getInputTypes() {
		return inputTypes;
	}

	/**
	 * Add another input type.
	 * 
	 * @param inputType
	 *            the additional type.
	 */
	public void addInputType(Type inputType) {
		inputType.setTestData(testdata);
		this.inputTypes.add(inputType);
	}

	/**
	 * Get the name of the function.
	 * 
	 * @return the name of the function.
	 */
	public String getName() {
		return name;
	}

	/**
	 * Get the type of the result.
	 * 
	 * @return the type of the result.
	 */
	public Type getResultType() {
		return resultType;
	}

	/**
	 * Set the type of the result.
	 * 
	 * @param resultType
	 *            type of the result.
	 */
	public void setResultType(Type resultType) {
		resultType.setTestData(testdata);
		this.resultType = resultType;
	}

	/**
	 * Set the method which will be called before executing the test.
	 * 
	 * @param setup
	 *            setup method.
	 */
	public void setSetup(Function setup) {
		setup.setTestData(testdata);
		this.setup = setup;
	}

	/**
	 * Set the method which will be called after executing the test to do some
	 * cleanup.
	 * 
	 * @param teardown
	 *            teardown method.
	 */
	public void setTeardown(Function teardown) {
		teardown.setTestData(testdata);
		this.teardown = teardown;
	}

	/**
	 * Get the setup method.
	 * 
	 * @return the setup method.
	 */
	public Function getSetup() {
		return setup;
	}

	/**
	 * Get the teardown method.
	 * 
	 * @return the teardown method.
	 */
	public Function getTeardown() {
		return teardown;
	}

	/**
	 * Get a list containing the testcases for this instance of the function.
	 * 
	 * @return the testcases.
	 */
	public List<TestCase> getTests() {
		return testCases;
	}

	/**
	 * Add another test to the testcases.
	 * 
	 * @param testCase
	 *            another testcase.
	 */
	public void addTest(TestCase testCase) {
		testCase.setTestData(testdata);
		testCase.setFunctionUnderTest(this);
		this.testCases.add(testCase);
	}

	/**
	 * Get an array of classes containg the types of the parameters.
	 * 
	 * @return an array with the parameter types.
	 * @throws ClassNotFoundException
	 */
	public Class[] getParams() throws ClassNotFoundException {
		Class[] params = new Class[getInputTypes().size()];
		int i = 0;
		for (Type arg : getInputTypes()) {
			params[i] = arg.getTypeClass();
			i++;
		}
		return params;
	}

	/**
	 * Set the object with testdata this instance belongs to.
	 * 
	 * @param td
	 *            the object this instance belongs to.
	 */
	public void setTestData(TestData td) {
		this.testdata = td;
	}

	@Override
	public String toString() {
		return toString("");
	}

	/**
	 * Pretty print some information on the function under test and the
	 * testcases.
	 * 
	 * @param indent
	 *            the indentation to use.
	 * @return the pretty string.
	 */
	public String toString(String indent) {
		StringBuffer sb = new StringBuffer();
		sb.append(indent + name + "(");
		for (Iterator inpt = inputTypes.iterator(); inpt.hasNext();) {
			Type element = (Type) inpt.next();
			sb.append(element);
			if (inpt.hasNext()) {
				sb.append(", ");
			}
		}
		sb.append(") :: " + resultType + "\n");
		sb.append(indent + "  Setup: " + setup + "\n");
		sb.append(indent + "  Teardown: " + teardown + "\n");
		for (TestCase testCase : testCases) {
			sb.append(testCase.toString(indent + "  "));
		}
		return sb.toString();
	}
}
