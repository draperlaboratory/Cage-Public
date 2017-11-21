package com.draper.cage.soot;

import soot.SootMethod;
import soot.Unit;

public class UnbalancedBranch
{
	public final SootMethod method;
	public final Unit branchStmt;
	public final Unit mergePoint;
	public final CostRange rangeToMergePoint;
	
	public UnbalancedBranch(SootMethod method,
		Unit branchStmt,
		Unit mergePoint,
		CostRange rangeToMergePoint)
	{
		this.method = method;
		this.branchStmt = branchStmt;
		this.mergePoint = mergePoint;
		this.rangeToMergePoint = rangeToMergePoint;
	}
	
	private static final String tab = "  ";

	@Override
	public String toString() {
		return new StringBuilder().append("unbalanced branch:\n")
		  .append(tab)
		  .append("method: ")
		  .append(method)
		  .append('\n')
		  .append(tab)
		  .append("branch statement: ")
		  .append(branchStmt)
		  .append('\n')
		  .append(tab)
		  .append("merge point: ")
		  .append(mergePoint)
		  .append('\n')
		  .append(tab)
		  .append("cost range to merge point: ")
		  .append(rangeToMergePoint)
		  .append('\n')
		  .append(tab)
		  .append("lower bound trace of cost range:\n")
		  .append(rangeToMergePoint.getLowerBound().getTrace())
		  .append('\n')
		  .append(tab)
		  .append("upper bound trace of cost range:\n")
		  .append(rangeToMergePoint.getUpperBound().getTrace())
		  .append('\n')
		  .toString();
	}
}
