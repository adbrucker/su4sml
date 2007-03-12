package ch.ethz.infsec.jtestdataaccessor.nodes;

/**
 * Represent a simple value such as int, boolean, String etc.
 * 
 * @author ms
 * 
 */
public class SimpleValue extends Value {

	private Object value;

	/**
	 * Create new {@link SimpleValue} object containing the given value.
	 * 
	 * @param value
	 *            the value of this node.
	 */
	public SimpleValue(Object value) {
		this.value = value;
	}

	/**
	 * Get the value.
	 * 
	 * @return gets the specified value.
	 */
	@Override
	public Object getValue() {
		return value;
	}

	@Override
	public String toString() {
		return toString("");
	}

	/**
	 * Pretty print the {@link SimpleValue} node.
	 */
	@Override
	public String toString(String indent) {
		return indent + value.toString();
	}

}