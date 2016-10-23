@//////////////////////////////////////////////////////////////////////////////
@// su4sml --- an SML repository for managing (Secure)UML/OCL models
@//             http://projects.brucker.ch/su4sml/
@//                                                                            
@// junit.tpl --- 
@// This file is part of su4sml.
@//
@// Copyright (c) 2005-2007, ETH Zurich, Switzerland
@//
@// All rights reserved.
@//
@// Redistribution and use in source and binary forms, with or without
@// modification, are permitted provided that the following conditions are
@// met:
@//
@//     * Redistributions of source code must retain the above copyright
@//       notice, this list of conditions and the following disclaimer.
@//
@//     * Redistributions in binary form must reproduce the above
@//       copyright notice, this list of conditions and the following
@//       disclaimer in the documentation and/or other materials provided
@//       with the distribution.
@//
@//     * Neither the name of the copyright holders nor the names of its
@//       contributors may be used to endorse or promote products derived
@//       from this software without specific prior written permission.
@//
@// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
@// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
@// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
@// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
@// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
@// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
@// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
@// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
@// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
@// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
@// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@///////////////////////////////////////////////////////////////////////////////
@// $Id$

@// Template for Junit TestCases

@foreach classifier_list
	@if isTestable
		@//---------------------------------------------------
		@// Generate main test class which inherits from the abstract class
		@//---------------------------------------------------
		@openfile generated/src/test/java/$classifier_package_path$/$classifier_name$Test.java
		package $classifier_package$;
		@nl@nl
		@nlimport java.io.FileNotFoundException;
		@nlimport org.junit.BeforeClass;
		@nlimport org.junit.Test;
		@nlimport ch.ethz.infsec.jtestdataaccessor.TestDataAccessor;
		@nlimport ch.ethz.infsec.jtestdataaccessor.TestDataParseException;
		@nlimport ch.ethz.infsec.jtestdataaccessor.TestDataUser;
		@nlimport ch.ethz.infsec.jtestdataaccessor.TestHelper;
		@nlimport antlr.RecognitionException;
		@nlimport antlr.TokenStreamException;
		@nlimport tudresden.ocl20.core.lib.*;
		@nlimport ch.ethz.infsec.jtestdataaccessor.oclexceptions.*;
		@nl
		@nlpublic class $classifier_name$Test extends Abstract$classifier_name$Test implements TestDataUser {

		@nl@tabpublic static String classUnderTest = "$classifier_package$.$classifier_name$";

		@nl@nl@tabprivate static TestDataAccessor tda;
	
		@nl@nl@tabprivate static TestHelper th;

		@nl@nl@tabpublic String getClassUnderTest() {
		@nl@tab@tabreturn classUnderTest;
		@nl@tab}
	
		@nl@nl@tabpublic Object getTestObject() {
		@nl@tab@tabreturn testObject;		
		@nl@tab}

		@nl@nl@tab\@BeforeClass
		@nl@tabpublic static void basicEnvSetup() throws FileNotFoundException, RecognitionException, TokenStreamException, TestDataParseException {
		@nl@tab@tabtda = new TestDataAccessor("src/test/resources/$classifier_package_path$/Testdata$classifier_name$");
		@nl@tab@tabth = new TestHelper(classUnderTest,tda);
		@nl@tab }
	
		@foreach unique_operation_list
			@if operation_isNotPrivate
				@nl@nl@tab\@Test
				@nl@tabpublic void $operation_name$Test() throws Throwable {
				@nl@tab@tabString methodname = "$operation_name$";
				@nl@tab@tabth.doTest(methodname, this);
				@nl@tab }
			@end
		@end
		@nl@nl
		@foreach operation_list
			 @nl@nl@tab 
			 /**@nl@tab
		 	 @spc* Wrapper to call $operation_name$ and check pre-/postconditions and invariants. @nl@tab
		 	 @spc*/
			@nl@tabpublic $operation_result_type$ wrapped_$operation_name$(
			@foreach argument_list
                        	@if last_argument
					$argument_type$ $argument_name$
                        	@else
					$argument_type$ $argument_name$,@spc
                        	@end
                	@end
                	) throws Throwable {@nl
			@if operation_non_void 
			    @tab@tab$operation_result_type$ result;@nl
			@end
			@tab@tab// Check preconditions @nl
			$preconditions$@nl
			@tab@tab// Execute method @nl@tab@tab
			@if operation_non_void 
			    result =@spc
			@end 
			testObject.$operation_name$(
			@foreach argument_list
				@if last_argument
					$argument_name$
				@else
					$argument_name$,@spc
				@end
			@end
			);@nl
			@tab@tab// Check postconditions @nl
			$postconditions$@nl
			@tab@tab// Check invariants @nl
			@tab@tabcheckInvariant();
			@nl@tab@tab
			@if operation_non_void
				return result;
			@end
			@nl@tab
			}@nl
		@end
		
		@nl@nl@tab 
		/**@nl@tab
		@spc* Check invariants of the class @nl@tab
		@spc*/@nl@tab
		public void checkInvariant() throws InvariantFailedException {@nl
		       $invariants$
		@nl@tab}

		@nl}

		@//--------------------------
		@// Generate stub for abstract class
		@//--------------------------
		@openfileifnotexists generated/src/test/java/$classifier_package_path$/Abstract$classifier_name$Test.java
		package $classifier_package$;
		@nl@nlimport $classifier_package$.$classifier_name$;
		@nl@nlpublic abstract class Abstract$classifier_name$Test {

		@nl@nl@tabstatic $classifier_name$ testObject; 

		@nl@nl}


		@//----------------------
		@// Generate stub for testdata
		@//----------------------
		@openfileifnotexists generated/src/test/resources/$classifier_package_path$/Testdata$classifier_name$

		@foreach operation_list
			@if operation_isNotPrivate
				[$operation_name$]
				@if operation_non_void
					@nlresulttype = $operation_result_type$;
				@end
				@if operation_has_arguments
					@nlinputtypes = @spc
					@foreach argument_list
		    				$argument_type$
		    				@if not_last_argument
							,@spc
		    				@end
					@end
					;
				@end
				@nl#setup = ;
				@nl#teardown = ;
				@nl#{
				@if operation_has_arguments
					@nl#@tab input = ;
				@end
				@if operation_non_void
					@nl#@tab result = ;
				@end
				@nl#@tab checker = ;
				@nl#@tab comment = ;
				@nl#}
				@nl@nl
			@end
		@end	 
	@end
@end

