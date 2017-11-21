package com.draper.cage.taint.flowanalysis;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import soot.Local;
import soot.jimple.Stmt;

/**
 * This class is used to show an explanation for the taintedness
 * of a tainted Local. This can happen for a variety of reasons.
 * Locals tainted by implicit taint are handled separately in 
 * ExplanationLocalImplicit.
 * @author rqg0530
 *
 */

public class ExplanationLocal extends Explanation {
				
	private Set<Local> explanation;
	
	private Stmt reason;
	
	/**
	 * Takes in a list of Values and adds them
	 * to the new objects explanation.
	 * @param explanation
	 */
	public ExplanationLocal(Set<Local> explanation, Stmt stmt) {
		this.explanation = explanation;
		this.reason = stmt;
	}
	
	/**
	 * Takes in a single Value and adds it to the list
	 * of Values in explanation.
	 * @param value
	 */
	public ExplanationLocal( Local explanation, Stmt stmt) {
		this.explanation = new HashSet<Local>();
		this.explanation.add(explanation);
		this.reason = stmt;
	}
	
	/**
	 * Returns the explanation.
	 * @return
	 */
	@Override
	public Set<Local> getExplanation() {
		return this.explanation;
	}
	
	@Override
	public Stmt getReason() {
		return this.reason;
	}
	
	@Override
	public String toString() {	
		return "Assign: " + this.reason.toString();
	}
	
	@Override
	public Map<String,String> printNodeEdge(String nodeName) {
		Map<String,String> retMap = new HashMap<>();
		if (!this.explanation.isEmpty()){
			for (Local explanation : this.explanation) {
				retMap.put(removeQuotes(explanation.toString()), removeQuotes(nodeName));
			}
		}
		
		return retMap;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((explanation == null) ? 0 : explanation.hashCode());
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ExplanationLocal other = (ExplanationLocal) obj;
		if (explanation == null) {
			if (other.explanation != null)
				return false;
		} else if (!explanation.equals(other.explanation))
			return false;
		return true;
	}
	
	
	
}
