package com.draper.cage.taint.flowanalysis;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import soot.Local;
import soot.SootMethod;
import soot.jimple.InstanceInvokeExpr;
import soot.jimple.InvokeExpr;
import soot.jimple.Stmt;

public class ExplanationLocalCall extends Explanation {
	
	private Set<ArgOrRetOrThis> explanation;
	
	private Stmt reason;

	public ExplanationLocalCall(Set<ArgOrRetOrThis> explanation, Stmt stmt) {
		this.explanation = explanation;
		this.reason = stmt;
	}

	@Override
	Set<ArgOrRetOrThis> getExplanation() {
		return this.explanation;
	}
	
	@Override
	Stmt getReason() {
		return this.reason;
	}
	
	SootMethod getMethod() {
		return this.reason.getInvokeExpr().getMethod();
	}

	@Override
	Map<String, String> printNodeEdge(String nodeName) {
		Map<String,String> retMap = new HashMap<>();
		retMap.put(removeQuotes(this.reason.toString()), removeQuotes(nodeName));
		return retMap;
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
				retString += "Tainted Base: " + eInstance.getBase() + "\n";
			}
			else {
				retString = "Tainted Return Value";
			}
			return retString;
		}
		return "Error: ExplanationCall does not contain InvokeExpr!";
	}

}
