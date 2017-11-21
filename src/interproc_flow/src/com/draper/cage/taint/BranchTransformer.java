package com.draper.cage.taint;
import java.util.List;
import java.util.Map;

import soot.Body;
import soot.BodyTransformer;
import soot.G;
import soot.PatchingChain;
import soot.Unit;
import soot.Value;
import soot.PhaseOptions;

/**
 * Calls method {@link #internalTransform(Body, String, Map)}
 * on the body of each method of the program.
 */
public class BranchTransformer extends BodyTransformer {

	public BranchTransformer(){
		super();
	}

	/**
	 * This method takes a look at the blocks that have
	 * the tainted flow and are branch statements.
	 * Then this method looks to set the conditions for
	 * the branch unit by switching them in for each unit.
	 */
	@Override
	protected void internalTransform(Body b, String phaseName, Map<String, String> options) {
		PatchingChain<Unit> units = b.getUnits();
		int switchIndices = getSwitchIndices(options);

		for (Unit u : units) {
			if(u.hasTag("isTaintedBranch")) {
				List<Value> switchValues = BranchTransform.getSwitchValues(u);
				int switchIndex = getSwitchIndex(u, switchIndices);
				BranchTransform.setCondition(u, switchValues.get(switchIndex));
				G.v().out.println("Changed to: " + u);
			}
		}
	}

	private static int getSwitchIndex(@SuppressWarnings("unused") Unit u, int switchIndices) {
		return switchIndices;
	}

	//Returns 0 if the flag is not passed
	private static int getSwitchIndices(Map<String, String> options) {
		return PhaseOptions.getInt(options, "switch-indices");
	}
}
