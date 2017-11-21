package com.draper.cage.taint.flowanalysis;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import soot.Scene;
import soot.SootMethod;

/**
 * This class is what stores the information regarding the summaries
 * of each method's runtime and taint analysis.
 * This object is passed throughout the file and stores the bulk
 * of the information in a single map data structure that associates
 * a method with a taint summary.
 * Implementation:
 * Field {@link #data} of this class allows for storing the
 * relationships between methods and their taint during runtime.
 * There are various access and manipulation methods to utilize this
 * data type.
 */
public class Summaries
{
	private Map<SootMethod, TaintSummary> data;

	public Summaries() {
		this.data = new HashMap<SootMethod, TaintSummary>();
	}

	public Summaries(Map<SootMethod, TaintSummary> data) {
		this.data = data;
	}

	/**
	 * simple put method that adds the method and taint summary data
	 * to the map of already established relationships
	 * @param method
	 * @param taintSummary
	 */
	public void put(SootMethod method, TaintSummary taintSummary) {
		this.data.put(method, taintSummary);

	}

	public boolean containsKey(SootMethod method) {
		return this.data.containsKey(method);
	}

	/**
	 * get method for finding the taint analysis associated with
	 * a certain method.
	 * @param method
	 * @return
	 */
	public TaintSummary get(SootMethod method) throws SummaryNotFoundException {
		if (this.data.containsKey(method)) {
			return this.data.get(method);
		}
		throw new SummaryNotFoundException("The method " + method + " has not been summarized!");
	}

	/**
	 * Returns the summary of the main method as defined by the
	 * Scene.
	 * @return
	 */
	//TODO: Carry around the user-given entry point!
	public TaintSummary getMainSummary() {
		return this.get(Scene.v().getMainMethod());
	}

	/**
	 * gets the summary associated with input method sm
	 * @param sm
	 * @return
	 */
	public TaintSummary getSummary(SootMethod sm){
		return this.get(sm);
	}

	/**
	 * outputs the data set in the map to a set data type that can be
	 * used for various applications throughout the analysis.
	 * @return
	 */
	public Set<Map.Entry<SootMethod, TaintSummary>> entrySet() {
		return this.data.entrySet();
	}

	@Override
	public String toString() {
		String s = new String("Summary of program behavior:\n\n");
		for (SootMethod m : this.data.keySet()) {
			s += "Method " + m + " :\n";
			s += this.data.get(m);
		}
		return s;	
	}
}
