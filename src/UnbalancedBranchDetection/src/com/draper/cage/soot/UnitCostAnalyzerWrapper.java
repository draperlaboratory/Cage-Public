package com.draper.cage.soot;

import soot.Unit;

public abstract class UnitCostAnalyzerWrapper implements UnitCostAnalyzer
{
	protected final UnitCostAnalyzer innerAnalyzer;
	
	public UnitCostAnalyzerWrapper(UnitCostAnalyzer innerAnalyzer) {
		this.innerAnalyzer = innerAnalyzer;
	}

	@Override
	public int getCost(Unit unit) {
		return innerAnalyzer.getCost(unit);
	}
}
