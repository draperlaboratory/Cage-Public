package com.draper.cage.soot;

import java.util.Comparator;
import java.util.List;
import java.util.Collection;
import java.util.Collections;

import soot.SootMethod;

public class MethodAnalysisResult
{
	private final SootMethod method;
	/**
	 * This list should only contain unbalanced branches
	 * found in method {@link #method}.
	 * */
	private final Collection<UnbalancedBranch> unbalancedBranches;
	
	public MethodAnalysisResult(SootMethod method,
			Collection<UnbalancedBranch> unbalancedBranches)
	{
		this.method = method;
		this.unbalancedBranches = unbalancedBranches;
	}
	
	@Override
	public String toString() {
		return new StringBuilder()
		  .append("unbalanced branch analysis result of method\n")
		  .append(method).append(":\n")
		  .append("# of unbalanced branches: ")
		  .append(unbalancedBranches.size())
		  .append('\n')
		  .append("most unbalanced branch: ")
		  .append(getMostUnbalancedBranch())
		  .toString();
	}
	
	private UnbalancedBranch getMostUnbalancedBranch() {
		int largestDiff = -1;
		UnbalancedBranch mostUnbalanced = null;
		for(UnbalancedBranch unbalancedb : unbalancedBranches) {
			if(mostUnbalanced == null) {
				mostUnbalanced = unbalancedb;
				largestDiff = getCostDiff(unbalancedb);
			}
			else {
				int costDiff = getCostDiff(unbalancedb);
				if(costDiff > largestDiff) {
					mostUnbalanced = unbalancedb;
					largestDiff = costDiff;
				}
			}
		}
		return mostUnbalanced;
	}
	
	private static int getCostDiff(UnbalancedBranch unbalancedb) {
		CostRange range = unbalancedb.rangeToMergePoint;
		return getCostDiff(range);
	}
	
	private static int getCostDiff(CostRange range) {
		Cost lowerBound = range.getLowerBound();
		Cost upperBound = range.getUpperBound();
		if(lowerBound instanceof IntegerCost &&
			upperBound instanceof IntegerCost)
		{
			int lowerCost = ((IntegerCost) lowerBound).getValue();
			int upperCost = ((IntegerCost) upperBound).getValue();
			return upperCost - lowerCost;
		}
		throw new IllegalArgumentException(
			"expected both lower and upper bounds of " +
			"input cost range to be integers");
	}
	
	private static int getLargestCostDiff(MethodAnalysisResult result) {
		UnbalancedBranch mostUnbalanced = result.getMostUnbalancedBranch();
		return getCostDiff(mostUnbalanced);
	}
	
	/**
	 * Used to sort MethodAnalysisResults in descending order of
	 * their cost difference of the most unbalanced branch
	 * @author jqa0822
	 *
	 */
	private static class LargestDiffComparator
		implements Comparator<MethodAnalysisResult>
	{
		@Override
		public int compare(MethodAnalysisResult r1, MethodAnalysisResult r2) {
			int largestDiff1 = getLargestCostDiff(r1);
			int largestDiff2 = getLargestCostDiff(r2);
			// largestDiff2 is the first argument of Integer.compare
			// because we want the order to be descending
			int compareDiffVal = Integer.compare(largestDiff2, largestDiff1);
			if(compareDiffVal == 0) {
				int numUnbalancedBranches1 = r1.unbalancedBranches.size();
				int numUnbalancedBranches2 = r2.unbalancedBranches.size();
				return Integer.compare(
					numUnbalancedBranches2, numUnbalancedBranches1);
			}
			return compareDiffVal;
		}
	}
	
	public static void sortByLargestDiff(List<MethodAnalysisResult> results) {
		Collections.sort(results, new LargestDiffComparator());
	}
}
