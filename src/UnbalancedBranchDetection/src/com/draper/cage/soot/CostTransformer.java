package com.draper.cage.soot;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import soot.Body;
import soot.BodyTransformer;
import soot.PatchingChain;
import soot.PhaseOptions;
import soot.SootMethod;
import soot.Unit;
import soot.jimple.Stmt;
import soot.jimple.toolkits.annotation.logic.Loop;
import soot.toolkits.graph.BriefUnitGraph;
import soot.toolkits.graph.MHGPostDominatorsFinder;
import soot.toolkits.graph.UnitGraph;

/**
 * This class will compute the heuristic cost of each method
 * in the input application.
 * For now, the cost will just be printed.
 * Later, the cost will be saved and passed to another phase.
 * @author jaltidor
 *
 */
public class CostTransformer extends BodyTransformer
{

	@Override
	protected void internalTransform(Body body,
		String phaseName, Map<String, String> options)
	{
		setDiffThreshold(options);
		SootMethod method = body.getMethod();
		UnitGraph unitGraph = new BriefUnitGraph(body);
		MHGPostDominatorsFinder<Unit> postDominatorFinder =
			new MHGPostDominatorsFinder<>(unitGraph);
		CostRangeAnalysis rangeAnalysis =
			new CostRangeAnalysis(unitGraph, body);
		List<UnbalancedBranch> unbalancedBranches =
			new LinkedList<>();
		Collection<Unit> nonLoopBranches = getNonLoopBranches(body);
		for(Unit branchStmt : nonLoopBranches) {
			Unit mergePoint =
				postDominatorFinder.getImmediateDominator(branchStmt);
			if(mergePoint != null) {
				CostRange rangeToMergePoint =
					rangeAnalysis.getCostRange(mergePoint);
				if(rangeToMergePoint.isLargeRange()) {
					UnbalancedBranch unbalancedBranch =
						new UnbalancedBranch(
							method,
							branchStmt,
							mergePoint,
							rangeToMergePoint);
					unbalancedBranches.add(unbalancedBranch);
				}
			}
		}
		if(!unbalancedBranches.isEmpty()) {
			MethodAnalysisResult result =
				new MethodAnalysisResult(method, unbalancedBranches);
			addMethodAnalysisResult(result);
		}
	}
	
	private static boolean isDiffThresholdSet = false;
	private static final int defaultDiffThreshold = 50;
	
	private synchronized static void setDiffThreshold(Map<String, String> options)
	{
		if(!isDiffThresholdSet) {
			String diffThresholdStr =
				PhaseOptions.getString(options, "diffthreshold");
			if(diffThresholdStr.isEmpty()) {
				IntegerCost.greatDiffThreshold = defaultDiffThreshold;
			}
			else {
				IntegerCost.greatDiffThreshold =
					Integer.parseInt(diffThresholdStr);
				isDiffThresholdSet = true;
			}
		}
	}
	
	private List<MethodAnalysisResult> methodAnalysisResults =
		new LinkedList<>();
	
	private synchronized void addMethodAnalysisResult(
		MethodAnalysisResult result)
	{
		methodAnalysisResults.add(result);
	}

	public List<MethodAnalysisResult> getMethodAnalysisResults() {
		return methodAnalysisResults;
	}
	
	private static Collection<Unit> getNonLoopBranches(Body body) {
		PatchingChain<Unit> units = body.getUnits();
		Collection<Unit> loopBranches = getLoopBranches(body);
		Collection<Unit> nonLoopBranches = new LinkedList<>();
		for(Unit unit : units) {
			if(isNonLoopBranch(unit, loopBranches)) {
				nonLoopBranches.add(unit);
			}
		}
		return nonLoopBranches;
	}
	
	
	private static Collection<Unit> getLoopBranches(Body body) {
		Collection<Loop> loops = Util.getLoops(body);
		Collection<Unit> loopBranches = new LinkedList<>();
		for(Loop loop : loops) {
			Collection<Stmt> loopStatements = loop.getLoopStatements();
			// first branching statement in loopStatements is the branching
			// statement of loop
			for(Stmt stmt : loopStatements) {
				if(stmt.branches()) {
					// Found loop branching statement
					loopBranches.add(stmt);
					break;
				}
			}
		}
		return loopBranches;
	}
	
	private static boolean isNonLoopBranch(Unit unit,
		Collection<Unit> loopBranches)
	{
		return isActualBranch(unit) && !isLoopBranch(unit, loopBranches);
	}
	
	private static boolean isActualBranch(Unit unit) {
		return unit.branches() && unit.fallsThrough();
	}

	private static boolean isLoopBranch(Unit unit,
		Collection<Unit> loopBranches)
	{
		return loopBranches.contains(unit);
	}
}
