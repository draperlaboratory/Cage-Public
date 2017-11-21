import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Stack;

import soot.G;
import soot.PointsToAnalysis;
import soot.SootMethod;
import soot.jimple.toolkits.annotation.purity.SootMethodFilter;
import soot.jimple.toolkits.callgraph.CallGraph;
import soot.jimple.toolkits.callgraph.Edge;

/**
 *
 * Borrows heavily from Cody's IflowAnalysis to produce an ordered list of class methods to analyze.
 * @author jthayer
 * 
 */
public class CallGraphTopo {

    private Stack<SootMethod> analyzeOrder;

	public CallGraphTopo(CallGraph cg, SootMethodFilter filter, Collection<SootMethod> heads,
				@SuppressWarnings("unused") PointsToAnalysis pa, @SuppressWarnings("unused") boolean verbose) {
		//initialize the stack of methods to analyze
		this.initializeToAnalyze(cg, filter, heads);
	}


	/**
	 * Initialize the toAnalyze work list of methods to analyze for a given application,
	 * keeping only methods reachable from `heads` that satisfy `filter`.
	 * @param cg
	 * @param filter
	 * @param heads
	 */
	protected void initializeToAnalyze(CallGraph cg, SootMethodFilter filter, Collection<SootMethod> heads) {
		this.analyzeOrder = new Stack<SootMethod>();
		//recursively fetch the methods called by each method
		Set<SootMethod> untreated = new HashSet<SootMethod>(heads);
		Set<SootMethod> treated   = new HashSet<SootMethod>();
		while (!untreated.isEmpty()) {
			SootMethod m = untreated.iterator().next();
			untreated.remove(m);
			treated.add(m);
			if (!filter.want(m)) {
				//Visit the children node, but do not add the method to analyzeOrder
				Iterator<Edge> mCalls = cg.edgesOutOf(m);
				while (mCalls.hasNext()) {
					SootMethod tgt = mCalls.next().tgt();
					if (!treated.contains(tgt)) {
						untreated.add(tgt);
					} else { /* Don't add an already treated method */ }
				}
			} else {
				assert (!this.analyzeOrder.contains(m));
				this.analyzeOrder.push(m);
				Iterator<Edge> mCalls = cg.edgesOutOf(m);
				while (mCalls.hasNext()) {
					SootMethod tgt = mCalls.next().tgt();
					if (!treated.contains(tgt)) {
						untreated.add(tgt);
					} else { /* Don't add an already treated method */ }
				}
			}
		}
		G.v().out.println("\nTo analyze: " + this.analyzeOrder.size() + " methods.\n");
	}

	/**
	 * Show the results of a successful analysis
	 */
	public void showResults(){
            G.v().out.println("The result of the topo sort on methods:\n");
            // TODO print methods in order to be analyzed.
	}


    // copy and return the set of methods to be analyzed.
    // we store the methods in a stack, because we arne't yet concerned with parallelism
    // eventually, we'll want a tree-like structure, a list of lists, that captures potential
    // parallelism in the processing of methods
    public Stack<SootMethod> getAnalysisOrder(){
    	Stack<SootMethod> toRet = new Stack<>();
    	toRet.addAll(this.analyzeOrder);
        return toRet;
    }
}
