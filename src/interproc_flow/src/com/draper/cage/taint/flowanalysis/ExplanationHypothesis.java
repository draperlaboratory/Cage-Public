package com.draper.cage.taint.flowanalysis;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import soot.jimple.Stmt;

/**
 * This class is used to show an explanation for the taintedness
 * of an object caused by ArgOrRetOrThis. It should be used
 * to form the 'root' of an ExplanationGraph.
 * @author rqg0530
 *
 */
public class ExplanationHypothesis extends Explanation {
	
	
	private Set<ArgOrRetOrThis> explanation;
	
	public ExplanationHypothesis(ArgOrRetOrThis arg) {
		this.explanation = new HashSet<>();
		this.explanation.add(arg);
	}
	
	@Override
	public Stmt getReason() {
		throw new UnsupportedOperationException("ExplanationHypothesis does not support getReason()");
	}

	
	@Override
	public String toString() {
		return "TaintAssumption: " + this.explanation.toString();
	}
	
	@Override
	Map<String,String> printNodeEdge(String nodeName) {
		Map<String,String> retMap = new HashMap<>();
		for (ArgOrRetOrThis a : explanation) {
			retMap.put(removeQuotes(a.toString()), nodeName);
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
		ExplanationHypothesis other = (ExplanationHypothesis) obj;
		if (explanation == null) {
			if (other.explanation != null)
				return false;
		} else if (!explanation.equals(other.explanation))
			return false;
		return true;
	}

	@Override
	Set<ArgOrRetOrThis> getExplanation() {
		// TODO Auto-generated method stub
		return this.explanation;
	}
	
	
	

}
