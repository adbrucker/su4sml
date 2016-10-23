/*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * BasicTest.java --- 
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
