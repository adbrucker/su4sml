package ch.ethz.infsec.jtestdataaccessor;

/**
 * Marker interface to mark a class which uses {@link TestData}.
 * 
 * @author ms
 * 
 */
public interface TestDataUser {
	public String getClassUnderTest();
	public Object getTestObject();
}
