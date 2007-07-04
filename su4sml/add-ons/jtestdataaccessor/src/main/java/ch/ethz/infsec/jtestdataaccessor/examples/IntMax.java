package ch.ethz.infsec.jtestdataaccessor.examples;

public class IntMax {
	public static int max(int[] array) throws Exception {
		int max = 0;
		for(int i : array){
			if(i < 0){
				throw new Exception("Negative element detected.");
			}
			if(i > max){
				max = i;
			}
		}
		return max;
	}
}
