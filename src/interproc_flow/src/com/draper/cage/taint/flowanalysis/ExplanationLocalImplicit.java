package com.draper.cage.taint.flowanalysis;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import soot.Context;
import soot.Unit;
import soot.jimple.Stmt;

/**
 * This class is used to show an explanation for the taintedness
 * of a Local caused by implicit taint.
 * @author rqg0530
 *
 */

public class ExplanationLocalImplicit extends Explanation {
				
	private Set<Unit> explanation;
	
	private Stmt reason;
	
	/**
	 * Takes in a list of Values and adds them
	 * to the new objects explanation.
	 * @param explanation
	 */
	public ExplanationLocalImplicit(Set<Unit> explanation, Stmt stmt) {
		this.explanation = explanation;
		this.reason = stmt;
	}
	
	/**
	 * Returns the explanation.
	 * @return
	 */
	@Override
	public Set<Unit> getExplanation() {
		return this.explanation;
	}
	
	@Override
	public Stmt getReason() {
		return this.reason;
	}
	
	@Override
	public String toString() {
		return "Implicit: " + this.explanation.toString();
	}
	
	@Override
	Map<String,String> printNodeEdge(String nodeName) {
		Map<String,String> retMap = new HashMap<>();
		for (Context c : this.explanation) {
			retMap.put(removeQuotes(c.toString()), removeQuotes(nodeName));
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
		ExplanationLocalImplicit other = (ExplanationLocalImplicit) obj;
		if (explanation == null) {
			if (other.explanation != null)
				return false;
		} else if (!explanation.equals(other.explanation))
			return false;
		return true;
	}

	
	
	
	
}
