package com.draper.cage.taint.flowanalysis;

import java.util.HashMap;
import java.util.Map;
import com.draper.cage.taint.RetValue;

import soot.Body;
import soot.SootMethod;
import soot.Unit;
import soot.toolkits.graph.DirectedGraph;
import soot.toolkits.graph.ExceptionalUnitGraph;

/**
 * This class serves as the main tool for initiating a taint analysis
 * of a particular method.
 * Initializes class {@link LocalFlowAnalysis} and invokes on a
 * particular method.
 * If this is not possible, then the default summary is given
 * to the method.
 * Implementation:
 * This class contains 2 methods:
 * {@link #summarizeMethod}, which is the main method that is used
 * for invoking the {@link LocalFlowAnalysis} workflow on a particular
 * method.
 * The method takes a SootMethod and a summaries object and then proceeds
 * to pass the number of arguments, the summaries, and the generated CFG
 * of the method into the LocalFlowAnalysis instance.
 * If the method is not analyzed by that method but still needs a summary,
 * then method {@link #summarizeUnanalyzedMethod(SootMethod)} generates
 * a default taint analysis summary and uses that for methods it does not
 * know about.
 */
public class MethodSummary
{
	public static TaintSummary summarizeMethod(SootMethod method, Summaries summaries) {
		Body body = method.retrieveActiveBody();
		DirectedGraph<Unit> cfg = new ExceptionalUnitGraph(body);
		
		LocalFlowAnalysis l = new LocalFlowAnalysis(method.getParameterCount(), summaries, cfg);
		return l.getResult();
	}

	/**
	 * Compute the taint for un-analyzed methods: if no arguments are
	 * tainted, then they remain so, and the return value is untainted.
	 * Otherwise, everything gets tainted.
	 * This is a heuristic, and may be unsound, but in most cases this
	 * is reasonable.
	 * By default, everything is modified by an un-analyzed method.
	 * @param method
	 * @return
	 */
	public static TaintSummary summarizeUnanalyzedMethod(SootMethod method) {
		int numArgs = method.getParameterCount();
		TaintSummary ret = new TaintSummary(SummarySource.AUTO);
		Map<ArgOrRetOrThis,Explanation> fullRet = new HashMap<ArgOrRetOrThis,Explanation>();
		Map<ArgOrRetOrThis,Explanation> allArgs = new HashMap<ArgOrRetOrThis,Explanation>();
		//Add every possible argument value to fullRet
		for (int i = 0; i < numArgs; i++){
			fullRet.put(ArgValue.v(i), new ExplanationHypothesis(ArgValue.v(i)));
			allArgs.put(ArgValue.v(i), new ExplanationHypothesis(ArgValue.v(i)));
		}
		//TODO: fix this explanation!
		fullRet.put(RetValue.v(), new ExplanationHypothesis(RetValue.v()));
		if (!method.isStatic()) {
			fullRet.put(ThisValue.v(), new ExplanationHypothesis(ThisValue.v()));
			allArgs.put(ThisValue.v(), new ExplanationHypothesis(ThisValue.v()));
		}
		//Set the unconditional taint to the empty set
		ret.setTaint(TaintAssumption.empty(),new HashMap<ArgOrRetOrThis,Explanation>());
		//Set the conditional taints to fullRet
		for (ArgOrRetOrThis a : allArgs.keySet()) {
			ret.setTaint(TaintAssumption.of(a), fullRet);
		}
		
		//Set the modified set to the full of arguments.
		ret.setModified(allArgs.keySet());
		return ret;
	}
	
	/**
	 * Create a summary for a source method. We use the following heuristic
	 * for source methods:
	 * 1. They are pure (no side effects)
	 * 2. They return a result that is tainted (e.g. the contents of a file,
	 * or the bytes read from a socket)
	 * @param method
	 * @return
	 */
	public static TaintSummary summarizeSourceMethod(SootMethod method){
		int numArgs = method.getParameterCount();
		TaintSummary ret = new TaintSummary(SummarySource.MANUAL);
		Map<ArgOrRetOrThis, Explanation> retTaint = new HashMap<>();
		//TODO: fix this explanation!
		retTaint.put(RetValue.v(), new ExplanationHypothesis(RetValue.v()));
		ret.setTaint(TaintAssumption.empty(), retTaint);
		for (int i = 0; i < numArgs; i++){
			ret.setTaint(TaintAssumption.of(ArgValue.v(i)), retTaint);
		}
		return ret;
	}
}
