package com.draper.cage.soot;

import java.util.Collection;
import soot.Unit;

public class UnitCostInLoopAnalyzer extends UnitCostAnalyzerWrapper
{
	private static final int loopFactor = 50;
	
	private final Collection<Unit> loopStatements;
	
	public UnitCostInLoopAnalyzer(Collection<Unit> loopStatements,
		UnitCostAnalyzer innerAnalyzer)
	{
		super(innerAnalyzer);
		this.loopStatements = loopStatements;
	}

	@Override
	public int getCost(Unit unit) {
		int innerCost = innerAnalyzer.getCost(unit);
		if(isInLoop(unit))
			return loopFactor * innerCost;
		return innerCost;
	}
	
	private boolean isInLoop(Unit unit) {
		return loopStatements.contains(unit);
	}
}
