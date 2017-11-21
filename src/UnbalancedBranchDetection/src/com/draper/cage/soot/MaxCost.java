package com.draper.cage.soot;

public class MaxCost implements Cost
{
	private MaxCost() {}
	
	public static final MaxCost INSTANCE = new MaxCost();

	@Override
	public int compareTo(Cost o) {
		// MaxCost is greater than every other Cost
		return 1;
	}

	@Override
	public Trace getTrace() {
		return Trace.getEmptyTrace();
	}

	@Override
	public Cost add(Cost other) {
		return this;
	}

	@Override
	public boolean differsGreatly(Cost other) {
		return true;
	}

	@Override
	public void accept(CostVisitor visitor) {
		visitor.visitMaxCost(this);
	}

	@Override
	public String toString() { return "+INFINITY"; }

}
