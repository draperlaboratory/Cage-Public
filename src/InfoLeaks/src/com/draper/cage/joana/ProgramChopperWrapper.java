package com.draper.cage.joana;

import java.util.Collection;
import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

public class ProgramChopperWrapper extends AbstractProgramChopper
{
	protected final ProgramChopper innerChopper;
	
	public ProgramChopperWrapper(ProgramChopper innerChopper) {
		this.innerChopper = innerChopper;
	}

	@Override
	public Collection<SDGNode> getChopNodes(SDGProgram program, SDGNode source,
		SDGNode sink)
	{
		return innerChopper.getChopNodes(program, source, sink);
	}
}
