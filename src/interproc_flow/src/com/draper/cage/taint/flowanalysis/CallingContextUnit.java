package com.draper.cage.taint.flowanalysis;
import soot.Context;
import soot.Unit;

/**
 * Class CallingContextUnit represents a unit and a series of
 * "contexts" which are a series of calls going from the method under
 * consideration to the statement through a series of invocations.
 * Implemented as a pure data-type
 * 
 * @author cdr1454
 *
 */
public interface CallingContextUnit {
	
	/**
	 * 
	 * @return the root unit
	 */
	public Unit getUnit();
	
	/**s
	 * @return the current context of the unit
	 */
	public Context getContext();
	
	/**
	 * 
	 * @return the body of a ContextCall, or the Unit of a ContextUnit
	 */
	public CallingContextUnit getBody();
}