package com.draper.cage.joana;

import java.util.Collection;

import edu.kit.joana.ifc.sdg.core.SecurityNode;
import edu.kit.joana.ifc.sdg.core.violations.IViolation;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

public class ProgramChop
{
	private final IViolation<SecurityNode> violation;
	private final SDGNode source;
	private final SDGNode sink;
	private final Collection<? extends SDGNode> nodes;
	
	public ProgramChop(IViolation<SecurityNode> violation,
		SDGNode source, SDGNode sink, Collection<? extends SDGNode> nodes)
	{
		this.violation = violation;
		this.source = source;
		this.sink = sink;
		this.nodes = nodes;
	}
	
	public IViolation<SecurityNode> getViolation() { return violation; }
	public SDGNode getSource() { return source; }
	public SDGNode getSink() { return sink; }
	public Collection<? extends SDGNode> getNodes() { return nodes; }
}
