package com.draper.cage.joana;

import java.util.Collection;

import edu.kit.joana.ifc.sdg.core.SecurityNode;
import edu.kit.joana.ifc.sdg.core.violations.IViolation;
import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

public interface ProgramChopper
{
	ProgramChop createChop(SDGProgram program, IViolation<SecurityNode> violation);
	
	Collection<SDGNode> getChopNodes(SDGProgram program, SDGNode source,
		SDGNode sink);
}
