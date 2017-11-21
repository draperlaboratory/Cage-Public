package com.draper.cage.taint.flowanalysis;
/**
 * This interface represents a possible argument position or the
 * return value or the this reference for a given method
 *
 */
public interface ArgOrRetOrThis
{
	boolean isRet();
	boolean isArg();
	boolean isThis();
	/** raises an exception if called on a return value */
	int getPos();
}
