package com.draper.cage.taint.flowanalysis;

import com.draper.cage.taint.MethodSet;

import soot.SootMethod;

public class SourceReader {

	/**
	 * Adds a summary for each element of the set of source methods to the map of summaries,
	 * using summarizeSourceMethod.
	 * 
	 * @param sum
	 * @param sources
	 */
	public static void addSources(Summaries sum, MethodSet sources) {
		for(SootMethod m : sources.get()) {
			sum.put(m, MethodSummary.summarizeSourceMethod(m));
		}
	}

}
