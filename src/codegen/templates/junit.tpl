@// Template for Junit TestCases

@foreach classifier_list
	@if isTestable
		@//---------------------------------------------------
		@// Generate main test class which inherits from the abstract class
		@//---------------------------------------------------
		@openfile generated/src/test/java/$classifier_package_path$/$classifier_name$Test.java
		package $classifier_package$;
		@nl@nl
		@nl import java.io.FileNotFoundException;
		@nl import org.junit.BeforeClass;
		@nl import org.junit.Test;
		@nl import ch.ethz.infsec.jtestdataaccessor.TestDataAccessor;
		@nl import ch.ethz.infsec.jtestdataaccessor.TestDataParseException;
		@nl import ch.ethz.infsec.jtestdataaccessor.TestDataUser;
		@nl import ch.ethz.infsec.jtestdataaccessor.TestHelper;
		@nl import antlr.RecognitionException;
		@nl import antlr.TokenStreamException;
		@nl
		@nl public class $classifier_name$Test extends Abstract$classifier_name$Test implements TestDataUser {

		@nl@tab public static String classUnderTest = "$classifier_package$.$classifier_name$";

		@nl@nl@tab private static TestDataAccessor tda;
	
		@nl@nl@tab private static TestHelper th;

		@nl@nl@tab public String getClassUnderTest() {
		@nl@tab@tab return classUnderTest;
		@nl@tab}
	
		@nl@nl@tabpublic Object getTestObject() {
		@nl@tab@tab return testObject;		
		@nl@tab}

		@nl@nl@tab \@BeforeClass
		@nl@tab public static void basicEnvSetup() throws FileNotFoundException, RecognitionException, TokenStreamException, TestDataParseException {
		@nl@tab@tab tda = new TestDataAccessor("src/test/resources/$classifier_package_path$/Testdata$classifier_name$");
		@nl@tab@tab th = new TestHelper(classUnderTest,tda);
		@nl@tab }
	
		@foreach operation_list
			@if operation_isNotPrivate
				@nl@nl@tab\@Test
				@nl@tab public void $operation_name$Test() throws Throwable {
				@nl@tab@tab String methodname = "$operation_name$";
				@nl@tab@tab th.doTest(methodname, this);
				@nl@tab }
			@end
		@end
		@nl}


		@//--------------------------
		@// Generate stub for abstract class
		@//--------------------------
		@openfileifnotexists generated/src/test/java/$classifier_package_path$/Abstract$classifier_name$Test.java
		package $classifier_package$;
		@nl import $classifier_package$.$classifier_name$;
		@nl@nl public abstract class Abstract$classifier_name$Test {

		@nl@nl@tab static Value testObject; 

		@nl@nl }


		@//----------------------
		@// Generate stub for testdata
		@//----------------------
		@openfileifnotexists generated/src/test/resources/$classifier_package_path$/Testdata$classifier_name$

		@foreach operation_list
			@if operation_isNotPrivate
				[$operation_name$]
				@nl resulttype = $operation_result_type$;
				@nl inputtypes = 
				@foreach argument_list
		    			$argument_type$
		    			@if not_last_argument
						,
		    			@end
				@end
				;
				@nl #setup = ;
				@nl #teardown = ;
				@nl #{
				@nl #@tab input = ;
				@nl #@tab result = ;
				@nl #@tab checker = ;
				@nl #@tab comment = ;
				@nl #}
				@nl@nl
			@end
		@end	 
	@end
@end
