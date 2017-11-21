package com.draper.cage.taint;
import soot.jimple.toolkits.callgraph.Edge;
import soot.jimple.toolkits.callgraph.EdgePredicate;

/**
 * Takes an edge and returns whether or not the class that calls
 * this method is an application class.
 */
public class ApplicationEdgePredicate implements EdgePredicate {

	/**
	 * Return true iff the declaring class of the source method
	 * of the edge is an application class 
	 */
	@Override
	public boolean want(Edge e) {
		return e.getSrc().method().getDeclaringClass().isApplicationClass();
	}
}
