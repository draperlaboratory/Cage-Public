package com.draper.cage.joana;

import java.io.IOException;
import java.util.Collection;

import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.ifc.sdg.graph.SDGNode;
import edu.kit.joana.ifc.sdg.graph.slicer.graph.CFG;
// import edu.kit.joana.ifc.sdg.util.sdg.GraphModifier;
import edu.kit.joana.ifc.sdg.util.graph.io.dot.MiscGraph2Dot;
import edu.kit.joana.ifc.sdg.util.sdg.ReducedCFGBuilder;

public class ChopPrinterDot implements ChopPrinter
{
	
	@Override
	public String getFileExtension() { return "dot"; }

	@Override
	public void printChop(SDGProgram program, String outputFileName,
		ProgramChop chop) throws IOException
	{
		SDG sdg = program.getSDG();
		SDG subgraph = sdg.subgraph((Collection<SDGNode>) chop.getNodes());
		CFG cfg = ReducedCFGBuilder.extractReducedCFG(subgraph);
		// GraphModifier.removeCallCallRetEdges(cfg);
		MiscGraph2Dot.export(cfg, MiscGraph2Dot.joanaGraphExporter(), outputFileName);
	}
}
