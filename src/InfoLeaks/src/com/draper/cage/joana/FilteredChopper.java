package com.draper.cage.joana;

import java.util.Collection;

import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

public abstract class FilteredChopper extends ProgramChopperWrapper
{
	public FilteredChopper(ProgramChopper innerChopper) {
		super(innerChopper);
	}
	
	public abstract boolean filter(SDGNode node);
	
	@Override
	public Collection<SDGNode> getChopNodes(SDGProgram program, SDGNode source,
		SDGNode sink)
	{
		Collection<? extends SDGNode> nodes =
			innerChopper.getChopNodes(program, source, sink);
		Collection<SDGNode> filteredNodes = new java.util.LinkedList<>();
		for(SDGNode node : nodes) {
			if(filter(node))
				filteredNodes.add(node);
		}
		return filteredNodes;
	}
}
