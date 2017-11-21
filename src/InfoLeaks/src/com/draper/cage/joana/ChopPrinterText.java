package com.draper.cage.joana;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;

import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

public class ChopPrinterText implements ChopPrinter
{
	@Override
	public String getFileExtension() { return "txt"; }

	@Override
	public void printChop(SDGProgram program, String outputFileName,
		ProgramChop chop) throws IOException
	{
		PrintWriter out = new PrintWriter(new FileWriter(outputFileName));
		Collection<? extends SDGNode> nodes = chop.getNodes();
		SDG sdg = program.getSDG();
		for(SDGNode node : nodes) {
			SDGNode method = sdg.getEntry(node);
			out.println(node.getLabel()
					+ "  |  " + node.getId()
					+ "  |  " + node.getOperation()
					+ "  |  " + method.getBytecodeMethod());
		}
		out.flush();
		out.close();
	}
}
