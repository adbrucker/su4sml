package ch.ethz.infsec.jtestdataaccessor.nodes;

import ch.ethz.infsec.jtestdataaccessor.nodes.SimpleValue;
import ch.ethz.infsec.jtestdataaccessor.nodes.Value;

public abstract class AbstractValueTest {

	static Value testObject; 

	/*
	public static void defaultSetup(){
		testObject = new Value();
	}
	*/
	
	public void setup1(){
		testObject = new SimpleValue("Hallo, Welt!"); 
	}
	
	public void setup2(){
		testObject = new SimpleValue("Hello, World!");
	}
	
}
