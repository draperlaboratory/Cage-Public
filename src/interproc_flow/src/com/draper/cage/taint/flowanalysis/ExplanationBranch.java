package com.draper.cage.taint.flowanalysis;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import soot.Local;
import soot.Value;
import soot.jimple.IfStmt;
import soot.jimple.Stmt;

/**
 * ExplanationBranch should be used to explain tainted branches.
 * An IfStmt with tainted Locals in the condition would be flagged
 * as tainted, added to the LocalSummary of a method, and this class
 * would store the Locals and the IfStmt.
 * @author rqg0530
 *
 */
public class ExplanationBranch extends Explanation {

	private Set<Local> explanation;
	
	private Stmt reason;
	
	public ExplanationBranch(Set<Local> taintedLocals, Stmt reason) {
		this.explanation = taintedLocals;
		this.reason = reason;
	}
	
	public ExplanationBranch(Local taintedLocal, Stmt reason) {
		this.explanation = new HashSet<>();
		this.explanation.add(taintedLocal);
		this.reason = reason;
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
		return "Locals: " + this.explanation.toString();
	}
	
	
	
	@Override
	Map<String,String> printNodeEdge(String nodeName) {
		Map<String,String> retMap = new HashMap<>();
		if (this.reason instanceof IfStmt) {
			Value ifStmt = ((IfStmt) this.reason).getCondition();
			Set<Local> locals = getLocals(ifStmt);
			for (Local l : locals) {
				retMap.put(removeQuotes(l.toString()), removeQuotes(nodeName));
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
		ExplanationBranch other = (ExplanationBranch) obj;
		if (explanation == null) {
			if (other.explanation != null)
				return false;
		} else if (!explanation.equals(other.explanation))
			return false;
		return true;
	}
	
	
	
}
