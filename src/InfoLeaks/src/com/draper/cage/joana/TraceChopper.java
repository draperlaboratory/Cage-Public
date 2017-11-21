package com.draper.cage.joana;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import edu.kit.joana.ifc.sdg.graph.JoanaGraph;
import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.ifc.sdg.graph.SDGEdge;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

public class TraceChopper extends AbstractProgramChopper
{
	@Override
	public Collection<SDGNode> getChopNodes(SDGProgram program, SDGNode source, SDGNode sink) {
		return createChop(program, source, sink);
	}
	
	public static Collection<SDGNode> createChop(SDGProgram program, SDGNode source, SDGNode sink) {
		SDG sdg = program.getSDG();
		Collection<SDGNode> trace = createTrace(sdg, source, sink);
		if(trace == null) // if no trace found
			return Collections.emptyList();
		else return trace;
	}
	
	private static List<SDGNode> createTrace(JoanaGraph graph, SDGNode source,
		SDGNode sink)
	{
		TreeSet<SDGNode> visited = new TreeSet<SDGNode>(SDGNode.getIDComparator());
		return createTrace(graph, source, sink, visited);
	}

	/**
	 * Perform backward search
	 * @param graph
	 * @param source
	 * @param sink
	 * @param visited
	 * @return
	 */
	private static List<SDGNode> createTrace(JoanaGraph graph, SDGNode source,
		SDGNode sink, Set<SDGNode> visited)
	{
		if(visited.contains(sink)) {
			return null;
		}
		visited.add(sink);
		if(isSameNode(source, sink)) {
			return Collections.singletonList(source);
		}
		Collection<SDGNode> predecessors = getPredecessors(graph, sink);
		for(SDGNode precedessor : predecessors) {
			List<SDGNode> prefixTrace =
				createTrace(graph, source, precedessor, visited);
			if(prefixTrace != null) { // if path found
				List<SDGNode> path = new LinkedList<>();
				path.addAll(prefixTrace);
				path.add(sink);
				return path;
			}
		}
		return null;
	}

	private static boolean isSameNode(SDGNode node1, SDGNode node2) {
		return node1.getId() == node2.getId();
	}
	
	private static Collection<SDGNode> getPredecessors(JoanaGraph graph, SDGNode node) {
		Set<SDGEdge> incomingEdges = getImportantEdges(graph, node);
		Collection<SDGNode> predecessors = new LinkedList<>();
		for(SDGEdge edge : incomingEdges) {
			predecessors.add(edge.getSource());
		}
		return predecessors;
	}

	private static Set<SDGEdge> getImportantEdges(JoanaGraph graph, SDGNode node) {
		Set<SDGEdge> incomingEdges = graph.incomingEdgesOf(node);
		Set<SDGEdge> importantEdges = new java.util.HashSet<>();
		for(SDGEdge edge : incomingEdges) {
			if(isImportantEdge(edge)) {
				importantEdges.add(edge);
			}
		}
		return importantEdges;
	}

	private static boolean isImportantEdge(SDGEdge edge) {
		SDGEdge.Kind kind = edge.getKind();
		if(!kind.isSDGEdge()) {
			return false;
		}
		if(kind.isThreadEdge()) {
			return false;
		}
		switch(kind) {
			case DATA_HEAP: return false;
			case SUMMARY: return false;
			default: return true;
		}
	}
}

