package com.draper.cage.joana;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import edu.kit.joana.api.sdg.SDGParameterUtils;
import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

public class TraceCleanerChopper extends ProgramChopperWrapper
{
	public TraceCleanerChopper(ProgramChopper innerChopper) {
		super(innerChopper);
	}
	
	@Override
	public Collection<SDGNode> getChopNodes(SDGProgram program, SDGNode source,
		SDGNode sink)
	{
		Collection<? extends SDGNode> trace =
			innerChopper.getChopNodes(program, source, sink);
		SDG sdg = program.getSDG();
		return cleanTrace(sdg, trace);
	}
	
	public static List<SDGNode> cleanTrace(SDG sdg,
			Collection<? extends SDGNode> trace)
	{
		List<SDGNode> importantNodes = mapToImportantNodes(sdg, trace);
		List<SDGNode> nonVirtualNodes = filterNonVirtualNodes(importantNodes);
		return noConsecutiveDuplicateNodes(nonVirtualNodes);
	}

	private static List<SDGNode> mapToImportantNodes(SDG sdg,
			Collection<? extends SDGNode> nodes)
	{
		List<SDGNode> importantNodes = new LinkedList<>();
		for(SDGNode node : nodes) {
			importantNodes.add(toImportantNode(sdg, node));
		}
		return importantNodes;
	}

	private static SDGNode toImportantNode(SDG sdg, SDGNode node) {
		switch(node.getOperation()) {
		case ACTUAL_IN:
			return SDGParameterUtils.locateCall(node, sdg);
		case ACTUAL_OUT:
			return SDGParameterUtils.locateCall(node, sdg);
		default:
			return node; 
		}
	}

	private static List<SDGNode> filterNonVirtualNodes(
		Collection<? extends SDGNode> nodes)
	{
		List<SDGNode> nonVirtualNodes = new LinkedList<>();
		for(SDGNode node : nodes) {
			if(ImportantFilteredChopper.isImportantNode(node)) {
				nonVirtualNodes.add(node);
			}
		}
		return nonVirtualNodes;
	}

	private static List<SDGNode> noConsecutiveDuplicateNodes(
		Collection<? extends SDGNode> nodes)
	{
		return noConsecutiveDuplicates(nodes, SDGNode.getIDComparator());
	}

	public static <T> List<T> noConsecutiveDuplicates(
		Collection<? extends T> collec, Comparator<? super T> comp)
	{
		if(collec.isEmpty()) {
			return Collections.emptyList();
		}
		Iterator<? extends T> itr = collec.iterator();
		T previous = itr.next();
		List<T> newList = new LinkedList<>();
		newList.add(previous); // adding first element
		while(itr.hasNext()) {
			T current = itr.next();
			if(comp.compare(previous, current) != 0) { // if previous != current
				newList.add(current);
				previous = current;
			}
		}
		return newList;
	}
}
