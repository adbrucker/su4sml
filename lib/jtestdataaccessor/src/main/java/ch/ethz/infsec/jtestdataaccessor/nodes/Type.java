package ch.ethz.infsec.jtestdataaccessor.nodes;

import static org.junit.Assert.fail;

import java.util.HashMap;

/**
 * Representation of a type given in the testdatafile.
 * 
 * @author ms
 * 
 */
public class Type extends Value {

	private String name;

	private boolean isArray;

	private static final HashMap<String, Class> basicTypeMap = new HashMap<String, Class>();

	private static final HashMap<String, Class> basicTypeArrayMap = new HashMap<String, Class>();

	/**
	 * Create a new {@link Type} object.
	 * 
	 * @param name
	 */
	public Type(String name) {
		this.name = name;
		if (basicTypeMap.size() == 0) {
			basicTypeMap.put(Boolean.TYPE.getName(), Boolean.TYPE);
			basicTypeMap.put(Byte.TYPE.getName(), Byte.TYPE);
			basicTypeMap.put(Character.TYPE.getName(), Character.TYPE);
			basicTypeMap.put(Short.TYPE.getName(), Short.TYPE);
			basicTypeMap.put(Integer.TYPE.getName(), Integer.TYPE);
			basicTypeMap.put(Long.TYPE.getName(), Long.TYPE);
			basicTypeMap.put(Float.TYPE.getName(), Float.TYPE);
			basicTypeMap.put(Double.TYPE.getName(), Double.TYPE);
		}
		if (basicTypeArrayMap.size() == 0) {
			basicTypeArrayMap.put(Boolean.TYPE.getName(), boolean[].class);
			basicTypeArrayMap.put(Byte.TYPE.getName(), byte[].class);
			basicTypeArrayMap.put(Character.TYPE.getName(), char[].class);
			basicTypeArrayMap.put(Short.TYPE.getName(), short[].class);
			basicTypeArrayMap.put(Integer.TYPE.getName(), int[].class);
			basicTypeArrayMap.put(Long.TYPE.getName(), long[].class);
			basicTypeArrayMap.put(Float.TYPE.getName(), float[].class);
			basicTypeArrayMap.put(Double.TYPE.getName(), double[].class);
		}
	}

	/**
	 * Get the {@link Class} object of this type. Distinguish between array and
	 * normal types and try to prepend java.lang if the class is not found at
	 * first.
	 * 
	 * @return the {@link Class} object.
	 * @throws ClassNotFoundException
	 */
	public Class getTypeClass() throws ClassNotFoundException {
		Class typeClass = null;
		if (basicTypeMap.get(name) != null) {
			if (isArray) {
				typeClass = basicTypeArrayMap.get(name);
			} else {
				typeClass = basicTypeMap.get(name);
			}
		} else {
			try {
				typeClass = Class.forName((isArray ? "[L" : "") + name);
			} catch (ClassNotFoundException e0) {
				try {
					typeClass = Class.forName((isArray ? "[L" : "")
							+ "java.lang." + name);
				} catch (ClassNotFoundException e1) {
					throw new ClassNotFoundException("Class " + name
							+ " not found!", new ClassNotFoundException(e1
							.getMessage(), new ClassNotFoundException(e0
							.getMessage())));
				}
			}
		}
		return typeClass;
	}

	@Override
	public String toString() {
		return toString("");
	}

	/**
	 * Pretty print this type.
	 * 
	 * @return the given type name.
	 */
	@Override
	public String toString(String indent) {
		return indent + name;
	}

	/**
	 * Return the class object, catching exceptions and failing if one occurs.
	 * 
	 * @return the class object representing the type.
	 */
	@Override
	public Class getValue() {
		try {
			return getTypeClass();
		} catch (ClassNotFoundException e) {
			fail(e.getMessage());
		}
		return null;
	}

	/**
	 * Is this type an array?
	 * 
	 * @return
	 */
	public boolean isArray() {
		return isArray;
	}

	/**
	 * Set this type to be an array.
	 * 
	 * @param isArray
	 */
	public void setArray(boolean isArray) {
		this.isArray = isArray;
	}

}
