package com.draper.cage.joana;

import java.util.Collection;

import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

public class WithinRangeChopper extends ProgramChopperWrapper
{
	public WithinRangeChopper(ProgramChopper innerChopper) {
		super(innerChopper);
	}
	
	@Override
	public Collection<SDGNode> getChopNodes(SDGProgram program, SDGNode source,
		SDGNode sink)
	{
		Collection<? extends SDGNode> nodes =
			innerChopper.getChopNodes(program, source, sink);
		int sourceId = source.getId();
		int sinkId = sink.getId();
		Collection<SDGNode> filteredNodes = new java.util.LinkedList<>(); 
		for(SDGNode node : nodes) {
			int nodeId = node.getId();
			if(nodeId >= sourceId && nodeId <= sinkId)
				filteredNodes.add(node);
		}
		return filteredNodes;
	}
}
