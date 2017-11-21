package com.draper.cage.joana;

import java.util.Collection;
import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.ifc.sdg.graph.SDGNode;
import edu.kit.joana.ifc.sdg.graph.chopper.Chopper;
import edu.kit.joana.ifc.sdg.graph.chopper.NonSameLevelChopper;

public class StandardChopper extends AbstractProgramChopper
{
	@Override
	public Collection<SDGNode> getChopNodes(SDGProgram program,
		SDGNode source, SDGNode sink)
	{
		return createChop(program, source, sink);
	}
	
	public static Collection<SDGNode> createChop(SDGProgram program,
		SDGNode source, SDGNode sink)
	{
		SDG sdg = program.getSDG();
		Chopper chopper = new NonSameLevelChopper(sdg);
		return chopper.chop(source, sink);
	}
}
