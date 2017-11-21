package com.draper.cage.joana;

import java.io.IOException;
import java.util.Collection;

import edu.kit.joana.ifc.sdg.graph.JoanaGraph;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDGNode;
import edu.kit.joana.ifc.sdg.graph.slicer.graph.building.CallGraphBuilder;
import edu.kit.joana.ifc.sdg.io.dot.JoanaGraph2Dot;

public class ChopPrinterCallGraphDot implements ChopPrinter
{
	
	@Override
	public String getFileExtension() { return "dot"; }
	
	@Override
	public void printChop(SDGProgram program, String outputFileName,
		ProgramChop chop) throws IOException
	{
		SDG sdg = program.getSDG();
		SDG subgraph = sdg.subgraph((Collection<SDGNode>) chop.getNodes());
		JoanaGraph graphForDot =
			CallGraphBuilder.buildEntryGraph(subgraph);
		JoanaGraph2Dot.writeDotToFile(graphForDot, outputFileName);
	}
}
