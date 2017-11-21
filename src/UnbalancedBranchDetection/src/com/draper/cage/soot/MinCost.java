package com.draper.cage.soot;

public class MinCost implements Cost
{
	private MinCost() {}
	
	public static final MinCost INSTANCE = new MinCost();

	@Override
	public int compareTo(Cost o) {
		// MinCost is less than every other Cost
		return -1;
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
		visitor.visitMinCost(this);
	}
	
	@Override
	public String toString() { return "-INFINITY"; }

}
