package com.draper.cage.joana;

import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

public class AddAppEntryToSourceChopper extends ProgramChopperWrapper
{
	private final String entryMethod;
	
	public AddAppEntryToSourceChopper(String entryMethod,
		ProgramChopper innerChopper)
	{
		super(innerChopper);
		this.entryMethod = entryMethod;
	}
	
	@Override
	public Collection<SDGNode> getChopNodes(SDGProgram program, SDGNode source,
		SDGNode sink)
	{
		SDG sdg = program.getSDG();
		SDGNode appEntry = getMainMethod(sdg);
		Collection<SDGNode> rootToSourceChop =
			innerChopper.getChopNodes(program, appEntry, source);
		Collection<? extends SDGNode> innerNodes =
			innerChopper.getChopNodes(program, source, sink);
		LinkedList<SDGNode> nodes = new LinkedList<>();
		nodes.addAll(rootToSourceChop);
		nodes.addAll(innerNodes);
		return nodes;
	}
	
	private SDGNode getMainMethod(SDG sdg) {
		Map<SDGNode, Set<SDGNode>> byProc = sdg.sortByProcedures();
		for (SDGNode nEntry : byProc.keySet()) {
			if (nEntry.getBytecodeMethod().equals(entryMethod)) {
				return nEntry;
			}
		}
		return null;
	}
}
