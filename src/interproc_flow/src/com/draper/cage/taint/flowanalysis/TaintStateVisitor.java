package com.draper.cage.taint.flowanalysis;
import java.util.Set;

/**
 * Interface that defines a visitor method that visits a
 * TaintAssumption-tainted argument relationship from a TaintState
 *
 * @param <A> the type of things we are tracking taint for
 */
public interface TaintStateVisitor<A> {

	public void visit(TaintAssumption h, Set<A> tainted);
}
