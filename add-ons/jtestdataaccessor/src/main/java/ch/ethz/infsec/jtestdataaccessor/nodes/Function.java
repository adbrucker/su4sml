/*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * Function.java --- 
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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import static org.junit.Assert.fail;

/**
 * A function node for a function which is used to set up the environment or to
 * create some value.
 * 
 * @author ms
 * 
 */
public class Function extends Value {

	private String name;

	/**
	 * Initialize a new function.
	 * 
	 * @param name
	 *            of the new function.
	 */
	public Function(String name) {
		this.name = name;
	}

	/**
	 * Get the name of the function.
	 * 
	 * @return the name.
	 */
	public String getName() {
		return name;
	}

	/**
	 * Call the function on the object with the junit tests, discarding the
	 * result.
	 * 
	 * @throws Throwable
	 * 
	 */
	public void call() throws Throwable {
		getValue();
	}

	/**
	 * Call the function using the given parameter types and arguments on the
	 * object with the junit tests, discarding the result.
	 * 
	 * @throws Throwable
	 * 
	 */
	public void call(Class[] classes, Object[] objects) throws Throwable {
		getValue(classes, objects);
	}

	/**
	 * Call the function on the object with the junit tests, but do not discard
	 * the result.
	 * 
	 * @throws Throwable
	 */
	@Override
	public Object getValue() throws Throwable {
		return getValue((Class[]) null, (Object[]) null);
	}

	/**
	 * Call the function using the given parameter types and arguments on the
	 * object with the junit tests and return the result.
	 * 
	 * @param params
	 *            types of the parameters.
	 * @param args
	 *            arguments to call the function with.
	 * @return the value produced by the called function.
	 * @throws Throwable
	 */
	public Object getValue(Class[] params, Object[] args) throws Throwable {
		try {
			if (testdata != null && testdata.getTestDataAccessor() != null
					&& testdata.getTestDataAccessor().getTestDataUser() != null) {
				Class c = testdata.getTestDataAccessor().getTestDataUser()
						.getClass();
				Method m = c.getMethod(name, params);
				return m.invoke(testdata.getTestDataAccessor()
						.getTestDataUser(), args);
			} else {
				System.err
						.println("No TestDataUser found to execute method on!");
			}
		} catch (InvocationTargetException e) {
			// The called function has thrown an exception, so return that
			// exception instead of a InvocationTargetException which is not
			// really useful outside.
			throw e.getCause();
		} catch (Exception e) {
			// There seems to be an exception in our code, which should not
			// happen...
			e.printStackTrace();
			fail("Failed to execute the specified function " + getName() + ".");
		}
		return null;
	}

	@Override
	public String toString() {
		return toString("");
	}

	/**
	 * Pretty print the function name.
	 * 
	 * @return the pretty name.
	 */
	@Override
	public String toString(String indent) {
		return indent + name + "()";
	}

}
