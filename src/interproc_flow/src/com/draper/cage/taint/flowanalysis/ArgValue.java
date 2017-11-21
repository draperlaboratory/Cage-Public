package com.draper.cage.taint.flowanalysis;
import java.util.HashMap;
import java.util.Map;

import soot.jimple.ParameterRef;

/**
 * Represents an (abstract) argument of a method call.
 */
final public class ArgValue implements ArgOrRetOrThis {

	private static Map<Integer, ArgValue> instances = new HashMap<>();

	private int argPos;

	private ArgValue(int pos){
		this.argPos = pos;
	}

	/**
	 * Takes an int that represents the index of the argument value that
	 * is supposed to be associated with that value and sets that to be
	 * the argPos argument.
	 * @param i the index of the method actual argument value
	 * @return
	 */
	public static ArgValue v(int i) {
		if (instances.containsKey(i)){
			return instances.get(i);
		}
		ArgValue a = new ArgValue(i);
		instances.put(i, a);
		return a;
	}

	@Override
	public boolean isRet() {
		return false;
	}

	@Override
	public boolean isArg() {
		return true;
	}

	@Override
	public int getPos() {
		return this.argPos;
	}

	@Override
	public String toString() {
		return "arg" + this.argPos;
	}

	@Override
	public boolean isThis() {
		return false;
	}

	public static ArgOrRetOrThis v(ParameterRef rVal) {
		return v(rVal.getIndex());
	}
}
