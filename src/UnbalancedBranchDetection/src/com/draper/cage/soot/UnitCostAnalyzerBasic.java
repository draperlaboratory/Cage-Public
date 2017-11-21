package com.draper.cage.soot;

import soot.Unit;

public class UnitCostAnalyzerBasic implements UnitCostAnalyzer
{
	private final UnitCostSwitch costSwitch = new UnitCostSwitch();

	@Override
	public int getCost(Unit unit) {
		unit.apply(costSwitch);
		return costSwitch.getCost();
	}
}
