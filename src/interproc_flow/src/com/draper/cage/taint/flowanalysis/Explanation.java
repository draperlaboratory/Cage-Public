package com.draper.cage.taint.flowanalysis;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import soot.Local;
import soot.Value;
import soot.ValueBox;
import soot.jimple.Stmt;

/** 
 * 	Explanation class used to store explanations for tainted objects.
 *  Each Explanation has a Set<?> called explanation, that can be thought of as the
 *  immediate explanation for why the object with the Explanation is tainted. The 
 *  objects in this Set can be Locals, CallingContextUnits, or ArgOrRetOrThis. The
 *  ExplanationGraph class can be used to piece together tainted objects using their Explanations.
 *  For each object in a LocalSummary, there is an Explanation mapped to it. The objects
 *  in this Explanation can be found elsewhere in the LocalSummary. As a result, we can
 *  visualize a digraph connecting a tainted object to any objects it might taint.
 *  
 * 
 * @author rqg0530
 *
 */

abstract public class Explanation {
	
	/**
	 * Return the explanation of the tainted object.
	 * @return
	 */
	
	abstract Set<?> getExplanation();
		
	/**
	 * Given the name of the object that's tainted, and the method that it was called in,
	 * return a Map<String,String> that points from taint source->targets.
	 * @param nodeName
	 * @param mainMethod
	 * @return
	 */
	abstract Map<String,String> printNodeEdge(String nodeName);
	
	/** Gets the set of locals contained in `value`.
	 * 
	 * @param value
	 * @return
	 */
	public static Set<Local> getLocals(Value value) {
		Set<Local> res = new HashSet<Local>();
		if (value instanceof Local){
			res.add((Local) value);
		} else {
			for (ValueBox vb : value.getUseBoxes()) {
				Value v = vb.getValue();
				if (v instanceof Local) {
					res.add((Local) v);
				}
			}
		}
		return res;
	}
	
	abstract Stmt getReason(); 
	
	
	/**
	 * Remove quotation marks from a String.
	 * @param s
	 * @return
	 */
	public static String removeQuotes(String s) {
		String retString;
		if (s.contains("\"")) {
			retString = s.replaceAll("\"", "'");
		}
		else {
			retString = s;
		}
		return retString;
	}
	
}
