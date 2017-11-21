package com.draper.cage.taint;

import com.draper.cage.taint.flowanalysis.ArgOrRetOrThis;

/**
 * Represents the (abstract) return value of a function call
 */
final public class RetValue implements ArgOrRetOrThis
{
	private static RetValue v = new RetValue();

	private RetValue() {

	}

	public static RetValue v() {
		return v;
	}

	@Override
	public boolean isRet() {
		return true;
	}

	@Override
	public boolean isArg() {
		return false;
	}

	@Override
	public int getPos() {
		throw new UnsupportedOperationException("getPos: called on a return value!");
	}

	@Override
	public String toString() {
		return "ret";
	}

	@Override
	public boolean isThis() {
		return false;
	}
}
