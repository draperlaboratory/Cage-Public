package com.draper.cage.soot;

/**
 * Models performance (run time or space) cost of an execution
 * trace of a program.
 * Note that the start of trace does not have to be the first
 * instruction of the main method of a program.
 * The start instruction can be the start of a method, and
 * the end instruction of a trace can be the exit node of the
 * CFG of a method.
 * @author jqa0822
 *
 */
public interface Cost extends Comparable<Cost>
{
	public Trace getTrace();
	
	public Cost add(Cost other);
	
	/**
	 * Method for accepting a Cost visitor.
	 * A cost visitor is useful for implementing operations that
	 * require double-dispatch logic such as compareTo.
	 * 
	 */
	public void accept(CostVisitor visitor);

	/**
	 * 
	 * @param other
	 * @return true iff the cost of this object greatly differs
	 * from argument other
	 * 
	 */
	public boolean differsGreatly(Cost other);

}
