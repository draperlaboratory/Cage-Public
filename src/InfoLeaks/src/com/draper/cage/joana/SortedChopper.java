package com.draper.cage.joana;

import java.util.Collection;
import java.util.TreeSet;

import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

public class SortedChopper extends ProgramChopperWrapper
{
	public SortedChopper(ProgramChopper innerChopper) {
		super(innerChopper);
	}
	
	@Override
	public Collection<SDGNode> getChopNodes(SDGProgram program, SDGNode source,
		SDGNode sink)
	{
		Collection<? extends SDGNode> nodes =
			innerChopper.getChopNodes(program, source, sink);
		TreeSet<SDGNode> ordered = new TreeSet<SDGNode>(SDGNode.getIDComparator());
		ordered.addAll(nodes);
		return ordered;
	}
}
