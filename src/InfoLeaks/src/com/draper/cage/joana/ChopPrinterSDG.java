package com.draper.cage.joana;

import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Collection;

import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.ifc.sdg.graph.SDGNode;
import edu.kit.joana.ifc.sdg.graph.SDGSerializer;
import edu.kit.joana.ifc.sdg.util.sdg.ReducedCFGUtil;

public class ChopPrinterSDG implements ChopPrinter
{
	@Override
	public String getFileExtension() { return "pdg"; }	

	@Override
	public void printChop(SDGProgram program, String outputFileName,
		ProgramChop chop) throws IOException
	{
		Collection<? extends SDGNode> subGraphNodes = chop.getNodes();
		SDG sdg = program.getSDG();
		SDG subgraph = sdg.subgraph((Collection<SDGNode>) subGraphNodes);
		ReducedCFGUtil.prepareForViewer(subgraph);
		FileOutputStream fOut = new FileOutputStream(outputFileName);
		SDGSerializer.toPDGFormat(subgraph, fOut);
		fOut.close();
	}
}
