package com.draper.cage.taint.flowanalysis;

/**
 * This class represents the "this" value of the object that is being
 * referenced in a method.
 * It also implements interface ArgOrRetOrThis so that the value can
 * be identified as the "this" value.
 * Implementation: ThisValue contains a field that is used for
 * instantiation of the ThisValue class.
 * It utilizes the .v() method to return instances of the class for
 * use in the program.
 * The class also implements the methods that are declared in the
 * abstract ArgOrRetOrThis to make sure that the object is identifiable.
 */
public class ThisValue implements ArgOrRetOrThis
{
	private ThisValue () { }

	private static ThisValue v = new ThisValue();

	public static ThisValue v() {
		return v;
	}

	@Override
	public boolean isRet() {
		return false;
	}

	@Override
	public boolean isArg() {
		return false;
	}

	@Override
	public boolean isThis() {
		return true;
	}

	@Override
	public int getPos() {
		throw new UnsupportedOperationException("getPos called on ThisValue!");
	}

	@Override
	public String toString(){
		return "this";
	}
}
