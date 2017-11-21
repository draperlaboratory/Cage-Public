package com.draper.cage.taint.flowanalysis;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import soot.Unit;

/**
 * The class representing the summary of the taint analysis for an
 * individual method.
 * Implementation:
 * The class contains a TaintState which records the
 * change in taint given a certain assumption as well as a set of
 * ArgOrRetOrThis that keeps track of all things that are modified.
 * The last field contains a set of branches that are tainted with
 * regards to a certain initial assumption which are stored in a
 * TaintState object.
 * @author cdr1454
 *
 */
public class TaintSummary {
	
	/**
	 * The origin for this summary: why/by whom the summary was created,
	 * e.g. by a heuristic, or manual annotation or program analysis
	 */
	private SummarySource source;

	/**
	 * Taint associations for arguments: ideally the tainted output
	 * would depend on an arbitrary set of tainted inputs, but to stay
	 * tractable we only treat 2 cases:
	 * taint occurring regardless of input,
	 * and taint occurring when a given individual variable is tainted.
	 */
	protected TaintState<ArgOrRetOrThis> taint;

	/**
	 * Set of arguments modified by the method.
	 * Useful when computing implicit taint
	 */
	protected Set<ArgOrRetOrThis> modified;

	//Taint for global values
	//private Set<Value> uncondGlob;
	//private Set<Value>[] globalDeps;

	/**
	 * Branches which may depend on tainted values. 
	 */
	protected TaintState<CallingContextUnit> resultTaintedBranches;

	/**
	 * Method calls which may be called with tainted data sources.
	 */
	protected TaintState<CallingContextUnit> resultTaintedCalls;

	public TaintSummary(SummarySource s){
		this.source = s;
		this.taint = new TaintState<>();
		this.modified = new HashSet<ArgOrRetOrThis>();
		this.resultTaintedBranches = new TaintState<>();
		this.resultTaintedCalls = new TaintState<>();
	}

	public TaintSummary(TaintSummary src) {
		this.copyFrom(src);
	}

	/**
	 * sets the modified variable to the set of arguments passed in
	 * to the function
	 * @param val
	 */
	public void setModified(Set<ArgOrRetOrThis> modified) {
		this.modified = modified;
	}

	/**
	 * @return the full set of modified arguments that were recorded
	 * during the analysis of a particular method.
	 */
	public Set<ArgOrRetOrThis> getModified() {
		return this.modified;
	}

	
	/**
	 * Wrapper for getUncondTaint
	 * @return this.taint.getUncondTaint()
	 */
	public Map<ArgOrRetOrThis,Explanation> getUncondTaint() {
		return this.taint.getUncondTaint();
	}
	
	/**
	 * Wrapper for this.taint.accept
	 * @param v Visitor for the taint field of this
	 */
	public void acceptTaintVisitor(TaintStateVisitor<ArgOrRetOrThis> v){
		this.taint.accept(v);
	}
	
	/**
	 * adds a modified value to the set of modified arguments that
	 * are recorded during the call of the method 
	 * @param val
	 */
	public void addModified(ArgOrRetOrThis val) {
		this.modified.add(val);
	}

	public TaintState<CallingContextUnit> getTaintedCalls() {
		return this.resultTaintedCalls;
	}
	
	public TaintState<CallingContextUnit> getTaintedBranches() {
		return this.resultTaintedBranches;
	}


	/**
	 * From a set of tainted arguments (or the `this` reference) as
	 * input, return the set of tainted arguments following the call
	 * of the method.
	 * Creates the set of modified arguments that are tainted during
	 * one run of a method by adding together the conditional and
	 * unconditional taint values.
	 * @param taintedArgs
	 * @return
	 */
	public Map<ArgOrRetOrThis,Explanation> applySummary(Set<ArgOrRetOrThis> taintedArgs) {
		Map<ArgOrRetOrThis,Explanation> retSet =
				new HashMap<ArgOrRetOrThis,Explanation>(this.taint.getUncondTaint());
		for (ArgOrRetOrThis a : taintedArgs) {
			retSet.putAll(this.taint.getCondTaint(a));
		}
		return retSet;
	}

	/** From a set of tainted arguments (or the `this` reference) as input,
	 * return the set of tainted branches within the body of the called method.
	 *
	 * 
	 * @param taintedArgs	the set of arguments that could be tainted at this particular callsite
	 * @param callSite		the callsite for the method summary that is being added to the set
	 * @return
	 */
	public Map<CallingContextUnit,Explanation> applyBranchSummary(Unit callSite, Set<ArgOrRetOrThis> taintedArgs, Explanation explanation){
		Map<CallingContextUnit,Explanation> retSet = new HashMap<>();
		Map<CallingContextUnit,Explanation>	uncondTaint = this.resultTaintedBranches.getUncondTaint();
		for (CallingContextUnit c : uncondTaint.keySet()) {
			retSet.put(new ContextCall(callSite, c), explanation);
		}
		for (ArgOrRetOrThis a : taintedArgs) {
			Map<CallingContextUnit,Explanation> condTaint = this.resultTaintedBranches.getCondTaint(a);
			for (CallingContextUnit c : condTaint.keySet()) {
				retSet.put(new ContextCall(callSite, c),explanation);
			}
		}
		return retSet;
	}


	/**
	 * 
	 * Takes the callsite and tainted args and adds all of method calls that are made by this
	 * method and sub-method calls and then creates a set of all calls with the context of the callsite 
	 * that they are made from.
	 * 
	 * @param callSite
	 * @param taintedArgs
	 * @return
	 */
	public Map<CallingContextUnit,Explanation> applyTaintedCall(Unit callSite, Set<ArgOrRetOrThis> taintedArgs, Explanation explanation) {
		Map<CallingContextUnit,Explanation> retSet = new HashMap<>();
		Map<CallingContextUnit,Explanation> uncondTaintCalls = this.resultTaintedCalls.getUncondTaint();
		for (CallingContextUnit c : uncondTaintCalls.keySet()) {
			retSet.put(new ContextCall(callSite, c), explanation);
		}
		for (ArgOrRetOrThis a : taintedArgs) {
			Map<CallingContextUnit,Explanation> condTaint = this.resultTaintedCalls.getCondTaint(a);
			for (CallingContextUnit c : condTaint.keySet()) {
				retSet.put(new ContextCall(callSite, c), explanation);
			}
		}
		return retSet;

	}

	/**
	 * Merges the taintSummaries of what is most likely to be
	 * 2 different paths of a certain method and combines the taint
	 * analysis with a join.
	 * The result is then put into a new taint summary.
	 * @param in1
	 * @param in2
	 * @param out
	 */
	protected static void merge(TaintSummary in1, TaintSummary in2, TaintSummary out) {
		TaintState.merge(in1.taint, in2.taint, out.taint);
		TaintState.merge(in1.resultTaintedBranches, in2.resultTaintedBranches,
				out.resultTaintedBranches);
		TaintState.merge(in1.resultTaintedCalls, in2.resultTaintedCalls, out.resultTaintedCalls);
		out.modified.addAll(in1.modified);
		out.modified.addAll(in2.modified);
	}

	private String uncondTaintToString() {
		String res = "[";
		Iterator<CallingContextUnit> itS = this.resultTaintedBranches.getUncondTaint().keySet().iterator();
		if (itS.hasNext()){
			res += itS.next();
			while (itS.hasNext()) {
				res += " , " + itS.next();
			}
		}
		res += "]";
		return res;
	}


	@Override
	public String toString() {
		String s = new String();
		s += "Taint:\n";
		s += this.taint.toString();
		s += "\nModified arguments:\n";
		s += this.modified.toString();
		s += "\nTainted Branches:\n";
		s += this.resultTaintedBranches.toString();
		s += "\nUnconditional Taint:\n";
		s += this.uncondTaintToString();
		s += "\nTainted Calls:\n";
		s += this.resultTaintedCalls.toString();
		s += "\n";
		return s;
	}

	/**
	 * sets the taint value for a certain assumption
	 * @param h
	 * @param taints
	 */
	public void setTaint(TaintAssumption h, Map<ArgOrRetOrThis, Explanation> taints) {
		this.taint.put(h, taints);

	}

	/**
	 * Adds a particular arg value to the set of taint for a
	 * particular assumption.
	 * @param h
	 * @param a
	 */
	public void addTaint(TaintAssumption h, ArgOrRetOrThis a, Explanation explanation) {
		this.taint.add(h, a, explanation);
	}
	
	public TaintState<ArgOrRetOrThis> getTaint() {
		return this.taint;
	}


	public void addTaintedBranch(TaintAssumption h, CallingContextUnit c, Explanation explanation){
		this.resultTaintedBranches.add(h, c, explanation);
	}

	public void addTaintedCall(TaintAssumption h, CallingContextUnit c, Explanation explanation) {
		this.resultTaintedCalls.add(h, c, explanation);
	}


	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.modified == null) ? 0 : this.modified.hashCode());
		result = prime * result + ((this.resultTaintedBranches == null) ? 0 : this.resultTaintedBranches.hashCode());
		result = prime * result + ((this.resultTaintedCalls == null) ? 0 : this.resultTaintedCalls.hashCode());
		result = prime * result + ((this.taint == null) ? 0 : this.taint.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		TaintSummary other = (TaintSummary) obj;
		if (this.modified == null) {
			if (other.modified != null)
				return false;
		} else if (!this.modified.equals(other.modified))
			return false;
		if (this.resultTaintedBranches == null) {
			if (other.resultTaintedBranches != null)
				return false;
		} else if (!this.resultTaintedBranches.equals(other.resultTaintedBranches))
			return false;
		if (this.resultTaintedCalls == null) {
			if (other.resultTaintedCalls != null)
				return false;
		} else if (!this.resultTaintedCalls.equals(other.resultTaintedCalls))
			return false;
		if (this.taint == null) {
			if (other.taint != null)
				return false;
		} else if (!this.taint.equals(other.taint))
			return false;
		return true;
	}

	/**
	 * Copies the taint information from another taint summary object
	 * @param src
	 */
	public void copyFrom(TaintSummary src) {
		this.source      = src.source;
		this.taint       = new TaintState<>(src.taint);
		this.modified    = new HashSet<ArgOrRetOrThis>(src.modified);
		this.resultTaintedBranches = new TaintState<>(src.resultTaintedBranches);
		this.resultTaintedCalls = new TaintState<>(src.resultTaintedCalls);
	}

	public SummarySource getSource() {
		return source;
	}

	public void setSource(SummarySource source) {
		this.source = source;
	}

}
