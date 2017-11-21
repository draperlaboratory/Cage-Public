package com.draper.cage.taint.flowanalysis;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import soot.Local;
import soot.jimple.InstanceInvokeExpr;
import soot.jimple.InvokeExpr;
import soot.jimple.Stmt;

/**
 * ExplanationCall should be used to Explain
 * tainted method calls. Because a method could
 * take 0 arguments, but the object it's being called
 * on could be tainted, we add a printCallEdge() to
 * determine if the object the method is being called on
 * is tainted. 
 * @author rqg0530
 *
 */
public class ExplanationCall extends Explanation {
	
	private Set<ArgOrRetOrThis> explanation;
	
	private Stmt reason;
	
	/**
	 * Takes in a list of Values and adds them
	 * to the new objects explanation.
	 * @param explanation
	 */
	public ExplanationCall(Set<ArgOrRetOrThis> explanation, Stmt stmt) {
		this.explanation = explanation;
		this.reason = stmt;
		}
	
	/**
	 * Takes in a single Value and adds it to the list
	 * of Values in explanation.
	 * @param value
	 */
	public ExplanationCall(ArgOrRetOrThis explanation, Stmt stmt) {
		this.explanation = new HashSet<ArgOrRetOrThis>();
		this.explanation.add(explanation);
		this.reason = stmt;
		}
	
	/**
	 * Returns the explanation.
	 * @return
	 */
	@Override
	public Set<ArgOrRetOrThis> getExplanation() {
		return this.explanation;
	}
	
	@Override
	public Stmt getReason() {
		return this.reason;
	}
	
	@Override
	public String toString() {
		Set<Local> taintedArgs = new HashSet<>();
		String retString = "";
		if (this.reason.containsInvokeExpr()) {
			InvokeExpr e = this.reason.getInvokeExpr();
			for (ArgOrRetOrThis arg : this.explanation) {
				if (arg.isArg()) {
					taintedArgs.add((Local)e.getArg(arg.getPos()));
				}
			}
			if (!taintedArgs.isEmpty()) {
				retString += "Tainted Args: " + taintedArgs + "\n";
			}
			if (this.explanation.contains(ThisValue.v())){
				InstanceInvokeExpr eInstance = (InstanceInvokeExpr) e;
				retString += "Tainted ThisValue: " + eInstance.getBase() + "\n";
			}
			else {
				retString = "Tainted Return Value";
			}
			return retString;
		}
		return "Error: ExplanationCall does not contain InvokeExpr!";
	}
	
	@Override
	Map<String,String> printNodeEdge(String nodeName) {
		/*
		 * The taintedLocals argument is needed just in case a Local in the InvokeExpr() is not
		 * tainted. We don't want to mark a Local as tainted if it isn't
		 */
		Map<String,String> retMap = new HashMap<>();
		if (this.reason.containsInvokeExpr()) {
			InvokeExpr e = this.reason.getInvokeExpr();
			for (ArgOrRetOrThis arg : this.explanation) {
				if (arg.isArg()) {
					retMap.putIfAbsent(removeQuotes(e.getArg(arg.getPos()).toString()), removeQuotes(nodeName));
				}
			}
			if (this.explanation.contains(ThisValue.v())) {
				retMap.putIfAbsent(removeQuotes(((InstanceInvokeExpr) e).getBase().toString()), removeQuotes(nodeName));
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
		result = prime * result + ((reason == null) ? 0 : reason.hashCode());
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
		ExplanationCall other = (ExplanationCall) obj;
		if (explanation == null) {
			if (other.explanation != null)
				return false;
		} else if (!explanation.equals(other.explanation))
			return false;
		if (reason == null) {
			if (other.reason != null)
				return false;
		} else if (!reason.equals(other.reason))
			return false;
		return true;
	}

	
	
	
}
