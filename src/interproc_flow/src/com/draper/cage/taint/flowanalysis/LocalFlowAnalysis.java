package com.draper.cage.taint.flowanalysis;
import java.util.LinkedHashSet;
import java.util.Set;

import soot.Unit;
import soot.toolkits.graph.DirectedGraph;
import soot.toolkits.graph.MHGPostDominatorsFinder;
import soot.toolkits.scalar.ForwardFlowAnalysis;

/**
 * This class bears the brunt of the work in the local analysis
 * of each individual method:
 * This class performs the forward flow analysis of the method that
 * is being analyzed and computes the implicit and explicit flows of
 * information throughout the body of a method.
 *
 */
public class LocalFlowAnalysis extends ForwardFlowAnalysis<Unit, LocalSummary>
{

	private int argNum;
	private DirectedGraph<Unit> cfg;
	private MHGPostDominatorsFinder<Unit> postDom;
	private Summaries summaries;

	public LocalFlowAnalysis(int argNum, Summaries summaries,
			DirectedGraph<Unit> cfg)
	{
		super(cfg);
		this.argNum = argNum;
		this.summaries = summaries;
		this.cfg = cfg;
		this.postDom = new MHGPostDominatorsFinder<Unit>(cfg);
		this.doAnalysis();
	}

	/**
	 * Takes the local summary before the expression that is to be analyzed
	 * and puts it into the output summary along with the information
	 * computed from the analysis
	 * 
	 * Do the case analysis on parameter {@code d}
	 * which computes the correct summary inside {@code out}.
	 * 
	 * @param in  The local summary before the expression
	 *   that is to be analyzed
	 * @param out This parameter will be set to the output summary along
	 *   with the information computed from the analysis
	 */
	@Override
	protected void flowThrough(LocalSummary in, Unit d, LocalSummary out) {
		out.copyFrom(in);
		d.apply(new UnitFlowSwitch(out, this.summaries));
		return;
	}

	/**
	 * Initializes the arguments taint as well as the taint of the "this"
	 * object in the analysis.
	 */
	@Override
	protected LocalSummary entryInitialFlow() {
		LocalSummary ret = new LocalSummary(this.argNum);
		Set<ArgOrRetOrThis> allArgs = new LinkedHashSet<>();
		for (int i = 0; i < ret.getNumArgs(); i++) {
			allArgs.add(ArgValue.v(i));
		}
		ret.addTaint(TaintAssumption.of(ThisValue.v()), ThisValue.v(), new ExplanationHypothesis(ThisValue.v()));
		//Initialize Each parameter to be tainted at output if it is tainted at input
		for (ArgOrRetOrThis a : allArgs) {
			ret.addTaint(TaintAssumption.of(a), a, new ExplanationHypothesis(a));
		}

		return ret;
	}

	/**
	 * merges 2 local summaries using the LocalSummary merge method
	 */
	@Override
	protected void merge(LocalSummary in1, LocalSummary in2, LocalSummary out) {
		LocalSummary.merge(in1, in2, out);
	}

	/**
	 * Removes the spurious implicit flows due to a join.
	 * We check, for each node `n` that engenders an implicit flow,
	 * that `d` is not a post-dominator to `n`, and remove `n` if it is.
	 * Implementation:
	 * Merges to reduce erroneous implicit flows due to a join.
	 * If there is an implicit flow we look to see if there is a
	 * post dominator d for that certain unit and remove it if that
	 * argument turns out to be a post dominator.
	 */
	@Override
	protected void mergeInto(Unit d, LocalSummary inout, LocalSummary in) {
		//G.v().out.println("Merging at node " + d.toString());
		this.merge(in, new LocalSummary(inout), inout);
		inout.removeBranches(d, this.postDom);
	}

	/**
	 * Copies a source summary into a destination summary
	 */
	@Override
	protected void copy(LocalSummary source, LocalSummary dest) {
		dest.copyFrom(source);
	}

	/**
	 * @return The merge result of the summaries of all tails or
	 * exit points of control flow graph {@link #cfg}.
	 */
	public TaintSummary getResult() {
		LocalSummary res = new LocalSummary(this.argNum);
		for (Unit u : this.cfg.getTails()) {
			LocalSummary nextSummary = this.getFlowAfter(u);
			this.merge(nextSummary, res, res);
		}
		return res;
	}

	@Override
	protected LocalSummary newInitialFlow() {
		return new LocalSummary(this.argNum);
	}

}
