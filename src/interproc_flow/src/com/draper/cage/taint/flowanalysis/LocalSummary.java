package com.draper.cage.taint.flowanalysis;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.draper.cage.taint.RetValue;

import java.util.Set;

import soot.Local;
import soot.Scene;
import soot.SootMethod;
import soot.Unit;
import soot.Value;
import soot.ValueBox;
import soot.jimple.InstanceInvokeExpr;
import soot.jimple.InvokeExpr;
import soot.jimple.Stmt;
import soot.jimple.toolkits.callgraph.CallGraph;
import soot.jimple.toolkits.callgraph.Edge;
import soot.toolkits.graph.MHGPostDominatorsFinder;

/** A local summary is a TaintSummary with the extra information it needs to 
 * propagate the taint information to the local variables.
 * Implementation:
 * The local summary extends the TaintSummary class and adds a set
 * of fields that are particularly useful for analysis of a method.
 * Field {@link #numArgs} is the number of arguments for the method.
 * Field {@link argAlias} maps arguments to the locals in the function
 * that are referencing it.
 * Field {@link taintedLocals} refers to the local variables that are
 * tainted under the assumption of taint at the beginning of the method.
 * Field {@link taintedBranches} represents the set of branches that are
 * tainted based on the data which flows through it
 * and on the data that affects the control flow.
 * 
 * @author cdr1454
 *
 */
public class LocalSummary extends TaintSummary
{
	/** The number of arguments of the method
	 */
	private int numArgs;

	/** Maps the arguments to (one of) the locals aliasing it, or null if there is no such local
	 */
	private Map<ArgOrRetOrThis, Local> argAlias;

	/** Represents the set of currently tainted locals 
	 * under the hypothesis that h is a tainted argument
	 */
	private TaintState<Local> taintedLocals;

	/** Represents the set of branches which depend on tainted data 
	 * which may influence the control of the current node.
	 */
	private TaintState<Stmt> taintedBranches;

	public LocalSummary(LocalSummary src) {
		super(SummarySource.ANALYSIS);
		this.copyFrom(src);
	}

	public LocalSummary(int numArgs) {
		super(SummarySource.ANALYSIS);
		this.numArgs               = numArgs;
		this.argAlias              = new LinkedHashMap<>();
		this.taintedLocals         = new TaintState<>();
		this.taintedBranches       = new TaintState<>();
	}

	public void addLocalAlias(ArgOrRetOrThis ref, Local l) {
		this.argAlias.put(ref, l);
	}

	/**
	 * Merges the values of 2 localSummaries, which is the join of
	 * 2 branches meeting again at the same code block.
	 * @param in1
	 * @param in2
	 * @param out represents the result of the merge of input
	 * local summaries in1 and in2.
	 */
	public static void merge(LocalSummary in1, LocalSummary in2, LocalSummary out) {
		//Call merge for the superclass
		TaintSummary.merge(in1, in2, out);
		out.argAlias = new LinkedHashMap<>(in1.argAlias);
		//Overwrite the aliases from in1
		out.argAlias.putAll(in2.argAlias);
		TaintState.merge(in1.taintedLocals, in2.taintedLocals, out.taintedLocals);
		TaintState.merge(in1.taintedBranches, in2.taintedBranches, out.taintedBranches);
	}

	/** 
	 * Removes all the branches which are post-dominated
	 * by input postDom according to postDom and input Unit d. 
	 * Utilizes a visitor method which removes the branch if argument
	 * node has been post-dominated.
	 * @param d
	 * @param postDom
	 */
	public void removeBranches(Unit d, MHGPostDominatorsFinder<Unit> postDom){

		TaintStateVisitor<Stmt> removeVisitor = new TaintStateVisitor<Stmt>(){

			@Override
			public void visit(TaintAssumption h, Set<Stmt> tainted) {
				Iterator<Stmt> it = tainted.iterator();
				while (it.hasNext()) {
					Stmt u = it.next();
					//Here isDominatedBy should be read isPostDominatedBy!
					if (postDom.isDominatedBy(u, d)){
						it.remove();
					}
				}				
			}
		};


		this.taintedBranches.accept(removeVisitor);

	}

	/** Gets the set of locals contained in `value`.
	 * 
	 * @param value
	 * @return
	 */
	private static Set<Local> getLocals(Value value) {
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

	/**
	 * Determines if a value is tainted given the current information
	 * available about the method.
	 * Checks if any of the variables associated with parameter
	 * value are tainted.
	 * 
	 * @param h
	 * @param value
	 * @return
	 */
	public boolean isTainted(TaintAssumption h, Value value) {
		Set<Local> valueLocals = LocalSummary.getLocals(value);
		// TODO Currently super unsound: ignores possible aliasing
		valueLocals.retainAll(this.taintedLocals.get(h).keySet());
		return !valueLocals.isEmpty();
	}

	/**
	 * Adds the fact that `value` is tainted to the current state
	 * under hypothesis:
	 * `hyp` add all relevant local variables to the set of tainted
	 * locals.
	 * Tag the the arguments which may reach this value as tainted.
	 * 
	 * @param h
	 * @param value
	 */
	public void addTaint(TaintAssumption h, Value value, Explanation explanation) {
		//G.v().out.println("Under assumption: " + h.toString());
		//G.v().out.println("Adding taint: " + value.toString());
		// TODO Currently super unsound: ignores possible aliasing
		Set<Local> valueLocals = LocalSummary.getLocals(value);
		Map<Local,Explanation> valueLocalsMap = new HashMap<Local,Explanation>();
		for (Local v : valueLocals) {
			valueLocalsMap.put(v, explanation);
		}
		for (Entry<ArgOrRetOrThis, Local> e : this.argAlias.entrySet()) {
			if (valueLocals.contains(e.getValue())) {
				if (!h.equals(TaintAssumption.empty())) {
					this.taint.add(h, e.getKey(), new ExplanationHypothesis(h.get()));
				}
				// TODO: Handle the case where we're under the None assumption
//				else {
//					this.taint.add(h, e.getKey(), new ExplanationHypothesis(TaintAssumption.empty()));
//				}
			}
		}
		this.taintedLocals.addAll(h, valueLocalsMap);

	}

	/** Add the fact that `value` is modified to the current state:
	 *  any argument which can reach the value is now considered modified.
	 * 
	 * @param value
	 */
	public void addModified(Value value) {
		// TODO Currently super unsound: ignores possible aliasing
		Set<Local> valueLocals = LocalSummary.getLocals(value);
		for (Entry<ArgOrRetOrThis, Local> e : this.argAlias.entrySet()) {
			if (valueLocals.contains(e.getValue())) {
				this.modified.add(e.getKey());
			}
		}

	}

	/**
	 * Determines whether there is implicit taint present in the
	 * current state of the method analysis
	 * 
	 * @param h
	 * @return
	 */
	public boolean hasImplicitTaint(TaintAssumption h) {
		return !this.taintedBranches.isEmpty(h);
	}

	/**
	 * Updates the taint information in the event of an invoke expression e
	 * with summary summaries(e).
	 * The summary determines the following:
	 * 1. Variables that are tainted after the call
	 * 2. Variables tainted by the implementation of the method call
	 * 3. Variables pointing to locations modified by the method,
	 *    if there is implicit taint.
	 * 4. Implicit flow that is recorded by any branches
	 * 
	 * @param h
	 * @param e
	 * @param summaries
	 * @return true if the return value of the method is tainted.
	 */
	public boolean addCall(TaintAssumption h, InvokeExpr e, Unit callSite, Summaries summaries, Explanation explanation) {
		//G.v().out.println("Under assumption: " + h.toString());
		//G.v().out.println("Analyzing call: " + e.toString());

		CallGraph cg = Scene.v().getCallGraph();
		//Compute the combined taint of all the methods that may be called at this
		// point
		TaintSummary eSummary = new TaintSummary(SummarySource.ANALYSIS);
		Iterator<Edge> edgesOut = cg.edgesOutOf(callSite);
		while(edgesOut.hasNext()) {
			SootMethod mayCall = edgesOut.next().tgt();
			TaintSummary mayCallSummary = summaries.get(mayCall);
			TaintSummary.merge(eSummary, mayCallSummary, eSummary);
		}

		List<Value> args = e.getArgs();
		int callArgNum = args.size();
		Set<ArgOrRetOrThis> outModified = eSummary.getModified();


		//Compute the argument positions that are tainted
		Set<ArgOrRetOrThis> taintedArgs = new HashSet<>();
		for (int i = 0; i < callArgNum; i++) {
			if (this.isTainted(h, args.get(i))) {
				taintedArgs.add(ArgValue.v(i));
			}
		}
		//Check to see if we are calling a non-static method
		if (e instanceof InstanceInvokeExpr) {
			Value base = ((InstanceInvokeExpr) e).getBase();
			if (this.isTainted(h, base)) {
				taintedArgs.add(ThisValue.v());
			}
		}

		// If any arguments are tainted, then add the current call to the set of tainted call-sites
		if (!taintedArgs.isEmpty()) {
			this.addTaintedCall(h, new ContextUnit(callSite), explanation);
		}


		Map<ArgOrRetOrThis, Explanation> outTainted = eSummary.applySummary(taintedArgs);
		Map<CallingContextUnit, Explanation> taintedBranches = eSummary.applyBranchSummary(callSite, taintedArgs, explanation);
		Map<CallingContextUnit, Explanation> taintedCalls = eSummary.applyTaintedCall(callSite, taintedArgs, explanation);


		/* If there is an implicit taint, use all the modified args, otherwise 
		 * just the tainted ones (unmodified tainted args stay tainted)
		 */
		Set<Value> modifiedOrTaintedArgs = new HashSet<Value>();
		if (this.hasImplicitTaint(h)) {
			for (ArgOrRetOrThis arg : outModified) {
				if (arg.isArg()) {
					modifiedOrTaintedArgs.add(args.get(arg.getPos()));
				} else if (arg.isThis()) {
					assert (e instanceof InstanceInvokeExpr);
					modifiedOrTaintedArgs.add(((InstanceInvokeExpr) e).getBase());
				} else {
					assert false; //The return value cannot be modified
				}
			}
		}
		else {
			for (ArgOrRetOrThis arg : outTainted.keySet()) {
				if (arg.isArg()) {
					modifiedOrTaintedArgs.add(args.get(arg.getPos()));
				} else if (arg.isThis()) {
					assert (e instanceof InstanceInvokeExpr);
					modifiedOrTaintedArgs.add(((InstanceInvokeExpr) e).getBase());
				} else {
					//The return value is tainted
				}				
			}
		}
		//Add all the tainted arguments to the set of tainted values
		for (Value v : modifiedOrTaintedArgs) {
			if (!this.taintedLocals.get(h).containsKey(v)) {
				if (v instanceof Local) {
					this.addTaint(h, v, new ExplanationLocal((Local) v, (Stmt) callSite));
				}
			}
		}

		//Add all the tainted branches in the call to the set of tainted branches
		for (CallingContextUnit c : taintedBranches.keySet()){
			this.addTaintedBranch(h, c, explanation);
		}

		//Add all the tainted calls in the current call context to the set of tainted calls
		for (CallingContextUnit c : taintedCalls.keySet()) {
			this.addTaintedCall(h, c, explanation);
		}

		//Compute the modified arguments
		Set<Value> modArgs = new HashSet<Value>();
		for (ArgOrRetOrThis a : outModified) {
			if (a.isArg()) {
				modArgs.add(args.get(a.getPos()));
			} else if (a.isThis()) {
				modArgs.add(((InstanceInvokeExpr) e).getBase());
			} else {
				//The return value can not be in the "modified" list
				assert false;
			}
		}

		for (Value v : modArgs) {
			this.addModified(v);
		}

		return outTainted.containsKey(RetValue.v());
	}


	/**
	 * Adds stmt to the collection of "currently" tainted branches,
	 * and to the global collection of tainted branches. 
	 * 
	 * @param h
	 * @param stmt
	 */
	public void addImplicitTaint(TaintAssumption h, Stmt stmt, Explanation explanation) {
		
		this.taintedBranches.add(h, stmt, explanation);
		this.resultTaintedBranches.add(h, new ContextUnit(stmt), explanation);
		
		
	}
	
	public Map<ArgOrRetOrThis,Local> getAlias() {
		return this.argAlias;
	}

	public TaintState<Local> getTaintedLocals() {
		return this.taintedLocals;
	}
	
	


	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.argAlias == null) ? 0 : this.argAlias.hashCode());
		result = prime * result + this.numArgs;
		result = prime * result + ((this.taintedBranches == null) ? 0 : this.taintedBranches.hashCode());
		result = prime * result + ((this.taintedLocals == null) ? 0 : this.taintedLocals.hashCode());
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
		LocalSummary other = (LocalSummary) obj;
		if (this.argAlias == null) {
			if (other.argAlias != null)
				return false;
		} else if (!this.argAlias.equals(other.argAlias))
			return false;
		if (this.numArgs != other.numArgs)
			return false;
		if (this.taintedBranches == null) {
			if (other.taintedBranches != null)
				return false;
		} else if (!this.taintedBranches.equals(other.taintedBranches))
			return false;
		if (this.taintedLocals == null) {
			if (other.taintedLocals != null)
				return false;
		} else if (!this.taintedLocals.equals(other.taintedLocals))
			return false;
		return true;
	}

	@Override
	public String toString() {
		String s = super.toString();
		return s + "[numArgs=" + this.numArgs + ", argAlias=" + this.argAlias + ", taintedLocals=" + this.taintedLocals + ", taintedBranches="
		+ this.taintedBranches + "]\n\n";
	}

	public int getNumArgs() {
		return this.numArgs;
	}

	public void copyFrom(LocalSummary src) {
		super.copyFrom(src);
		this.numArgs               = src.numArgs;
		this.argAlias              = new LinkedHashMap<>(src.argAlias);
		this.taintedLocals         = new TaintState<Local>(src.taintedLocals);
		this.taintedBranches       = new TaintState<Stmt>(src.taintedBranches);

	}
}
