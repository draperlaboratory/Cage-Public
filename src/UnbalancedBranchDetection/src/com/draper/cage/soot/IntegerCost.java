package com.draper.cage.soot;

public class IntegerCost implements Cost
{
	private final int value;
	private final Trace trace;
	
	// Configurations for cost range analysis
	
	/**
	 * The cost of a trace containing a loop will be calculated
	 * by multiplying the cost of executing each instruction in
	 * the trace by loopFactor
	 **/
	private static final int loopFactor = 5;
	
	/**
	 * Two integer costs will be considered substantially different
	 * if the absolute value of their difference is greater than
	 * greatDiffThreshold
	 **/
	public static int greatDiffThreshold = 70;
	
	public IntegerCost(int value, Trace trace) {
		this.value = value;
		this.trace = trace;
	}
	
	private static IntegerCost createEmptyCost() {
		return new IntegerCost(0, Trace.getEmptyTrace());
	}
	
	private static IntegerCost emptyCost = createEmptyCost();
	
	public static IntegerCost getEmptyCost() {
		return emptyCost;
	}

	@Override
	public Trace getTrace() { return trace; }

	public int getValue() { return value; }

	@Override
	public void accept(CostVisitor visitor) {
		visitor.visitIntegerCost(this);
	}
	
	@Override
	public String toString() { return Integer.toString(value); }
	
	private final ComparisonVisitor comparisonVisitor =
		new ComparisonVisitor();

	@Override
	public int compareTo(Cost other) {
		other.accept(comparisonVisitor);
		return comparisonVisitor.comparisonValue;
	}
	
	private class ComparisonVisitor implements CostVisitor
	{
		private int comparisonValue = 0;

		@Override
		public void visitIntegerCost(IntegerCost other) {
			int otherValue = other.value;
			int thisValue = IntegerCost.this.value;
			comparisonValue = thisValue - otherValue;
		}

		@Override
		public void visitMinCost(MinCost other) {
			comparisonValue = 1;
		}

		@Override
		public void visitMaxCost(MaxCost other) {
			comparisonValue = -1;
			
		}
	}

	@Override
	public Cost add(Cost other) {
		other.accept(additionVisitor);
		return additionVisitor.sum;
	}
	
	private final AdditionVisitor additionVisitor =
		new AdditionVisitor();
	
	private class AdditionVisitor implements CostVisitor
	{
		private Cost sum = null;

		@Override
		public void visitIntegerCost(IntegerCost other) {
			int thisValue = IntegerCost.this.value;
			int otherValue = other.value;
			int totalCost = thisValue + otherValue;
			Trace thisTrace = IntegerCost.this.getTrace();
			Trace otherTrace = other.getTrace();
			Trace combinedTrace = Trace.append(thisTrace, otherTrace);
			if(combinedTrace.containsLoop()) {
				totalCost *= loopFactor;
			}
			sum = new IntegerCost(totalCost, combinedTrace);
		}

		@Override
		public void visitMinCost(MinCost other) {
			sum = other;
		}

		@Override
		public void visitMaxCost(MaxCost other) {
			sum = other;
		}
	}
	
	@Override
	public boolean differsGreatly(Cost other) {
		other.accept(diffVisitor);
		return diffVisitor.isLargeDifference;
	}
	
	private final DiffVisitor diffVisitor =
			new DiffVisitor();
	
	private class DiffVisitor implements CostVisitor
	{
		private boolean isLargeDifference = false;

		@Override
		public void visitIntegerCost(IntegerCost other) {
			int diff =  Math.abs(compareTo(other));
			isLargeDifference = diff > greatDiffThreshold;
		}

		@Override
		public void visitMinCost(MinCost other) {
			isLargeDifference = true;
		}

		@Override
		public void visitMaxCost(MaxCost other) {
			isLargeDifference = true;
		}
	}

}
