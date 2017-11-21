package com.draper.cage.joana;

import java.util.Collection;
import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.ifc.sdg.graph.SDGNode;
import edu.kit.joana.ifc.sdg.graph.slicer.Slicer;
import edu.kit.joana.ifc.sdg.graph.slicer.SummarySlicerBackward;
import edu.kit.joana.ifc.sdg.graph.slicer.SummarySlicerForward;

public class SlicerChopper extends AbstractProgramChopper
{
	@Override
	public Collection<SDGNode> getChopNodes(SDGProgram program, SDGNode source,
		SDGNode sink)
	{
		return createChop(program, source, sink);
	}
	
	public static Collection<SDGNode> createChop(SDGProgram program, SDGNode source,
		SDGNode sink)
	{
		SDG sdg = program.getSDG();
		Slicer backwardSlicer =
			new SummarySlicerBackward(sdg);
		SDGNode backwardSeed = sink;
		Collection<SDGNode> backwardSlice =
			backwardSlicer.slice(backwardSeed);
		Slicer forwardSlicer =
			new SummarySlicerForward(sdg);
		SDGNode forwardSeed = source;
		Collection<SDGNode> forwardSlice = forwardSlicer.slice(forwardSeed);
		forwardSlice.retainAll(backwardSlice);
		return forwardSlice;
	}
}
