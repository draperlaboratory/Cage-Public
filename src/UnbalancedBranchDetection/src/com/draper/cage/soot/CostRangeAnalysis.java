package com.draper.cage.soot;

import java.util.Collection;
import java.util.LinkedList;

import soot.Body;
import soot.Unit;
import soot.jimple.toolkits.annotation.logic.Loop;
import soot.toolkits.scalar.ForwardFlowAnalysis;
import soot.toolkits.graph.DirectedGraph;

public class CostRangeAnalysis extends ForwardFlowAnalysis<Unit, CostRange>
{
	private final UnitCostAnalyzer costAnalyzer;
	
	public CostRangeAnalysis(DirectedGraph<Unit> cfg, Body body) {
		super(cfg);
		this.costAnalyzer = createUnitCostAnalyzer(body);
		this.doAnalysis();
	}
	
	private static UnitCostAnalyzer createUnitCostAnalyzer(Body body) {
		Collection<Unit> loopStatements = getLoopStatements(body);
		return new UnitCostInLoopAnalyzer(loopStatements,
			new UnitCostAnalyzerBasic());
	}
	
	private static Collection<Unit> getLoopStatements(Body body) {
		return getLoopStatements(Util.getLoops(body));
	}
	
	private static final Collection<Unit> getLoopStatements(
		Collection<Loop> loops)
	{
		LinkedList<Unit> stmts = new LinkedList<>();
		for(Loop loop : loops) {
			stmts.addAll(loop.getLoopStatements());
		}
		return stmts;
	}
	
	private Cost getCost(Unit unit) {
		int costOfUnit = costAnalyzer.getCost(unit);
		Trace trace = Trace.createSingletonTrace(unit);
		return new IntegerCost(costOfUnit, trace);
	}

	@Override
	protected void flowThrough(CostRange in, Unit d, CostRange out) {
		Cost costOfUnit = getCost(d);
		Cost inLowerBound = in.getLowerBound();
		Cost inUpperBound = in.getUpperBound();
		Cost outLowerBound = inLowerBound.add(costOfUnit);
		Cost outUpperBound = inUpperBound.add(costOfUnit);
		CostRange result = new CostRange(outLowerBound, outUpperBound);
		out.copyFrom(result);
	}
	
	/**
	 * @return the cost range immediately before the entry statement
	 * the zero cost range [0 - 0]
	 */
	@Override
    protected CostRange entryInitialFlow() {
		return new CostRange(
			IntegerCost.getEmptyCost(),
			IntegerCost.getEmptyCost());
    }

	/**
	 * @return the bottom element of the cost range lattice:
	 * the empty range [+INFINITY - -INFINITY]
	 */
	@Override
	protected CostRange newInitialFlow() {
		return new CostRange(MaxCost.INSTANCE, MinCost.INSTANCE);
	}
	
	@Override
	protected void merge(Unit u, CostRange in1, CostRange in2, CostRange out){
		CostRange union = CostRange.unionNoOccur(u, in1, in2);
		out.copyFrom(union);
	}
	
	@Override
	protected void copy(CostRange source, CostRange dest) {
		dest.copyFrom(source);
	}

	/**
	 * This method should only be called after invoking method
	 * {@link #doAnalysis()}.
	 * The user does not need to worry about this because
	 * method {@link #doAnalysis()} is invoked in the constructor
	 * of this class. 
	 * @param unit
	 * @return The range of execution cost from the entry node
	 * to this unit
	 */
	public CostRange getCostRange(Unit unit) {
		return this.getFlowAfter(unit);
	}

	@Override
	protected void merge(CostRange in1, CostRange in2, CostRange out) {
		throw new UnsupportedOperationException("Merge needs to be called with 4 arguements!");
		
	}
}
