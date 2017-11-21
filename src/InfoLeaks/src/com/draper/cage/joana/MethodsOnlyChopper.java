package com.draper.cage.joana;

import java.util.Collection;
import java.util.TreeSet;

import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

public class MethodsOnlyChopper extends ProgramChopperWrapper
{
	public MethodsOnlyChopper(ProgramChopper innerChopper) {
		super(innerChopper);
	}

	@Override
	public Collection<SDGNode> getChopNodes(SDGProgram program, SDGNode source,
		SDGNode sink)
	{
		Collection<? extends SDGNode> nodes =
			innerChopper.getChopNodes(program, source, sink); 
		TreeSet<SDGNode> methodNodes = new TreeSet<SDGNode>(SDGNode.getIDComparator());
		SDG sdg = program.getSDG();
		for(SDGNode node : nodes) {
			SDGNode entryMethod = sdg.getEntry(node);
			methodNodes.add(entryMethod);
		}
		return methodNodes;
	}
}
