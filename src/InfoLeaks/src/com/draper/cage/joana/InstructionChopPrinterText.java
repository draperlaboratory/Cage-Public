package com.draper.cage.joana;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.Comparator;

import edu.kit.joana.api.sdg.SDGInstruction;
import edu.kit.joana.api.sdg.SDGMethod;
import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

public class InstructionChopPrinterText implements ChopPrinter
{
	@Override
	public String getFileExtension() { return "txt"; }

	@Override
	public void printChop(SDGProgram program, String outputFileName,
		ProgramChop chop) throws IOException
	{
		PrintWriter out = new PrintWriter(new FileWriter(outputFileName));
		Collection<? extends SDGNode> nodes = chop.getNodes();
		Collection<SDGInstruction> instructionChop =
			instructionsOfNodes(program, nodes);
		Collection<SDGInstruction> instructionChopClean =
			TraceCleanerChopper.noConsecutiveDuplicates(
				instructionChop, new SDGInstructionComparator());
		for(SDGInstruction instr : instructionChopClean) {
			out.println(InfoFlowAnalysis.stringOfInstruction(instr));
		}
		out.flush();
		out.close();
	}
	
	private static Collection<SDGInstruction> instructionsOfNodes(
		SDGProgram program, Collection<? extends SDGNode> nodes)
	{
		Collection<SDGInstruction> instructions = new java.util.LinkedList<>();
		SDG sdg = program.getSDG();
		for(SDGNode n : nodes) {
			SDGMethod m = program.getMethod(sdg.getEntry(n).getBytecodeMethod());
			SDGInstruction i = m.getInstructionWithBCIndex(n.getBytecodeIndex());
			if (i != null) {
				instructions.add(i);
			}
		}
		return instructions;
	}
	
	private static class SDGInstructionComparator
		implements Comparator<SDGInstruction>
	{
		@Override
		public int compare(SDGInstruction instr1, SDGInstruction instr2) {
			SDGMethod meth1 = instr1.getOwner();
			SDGMethod meth2 = instr2.getOwner();
			String methSig1 = meth1.toString();
			String methSig2 = meth2.toString();
			int methSigCmp = methSig1.compareTo(methSig2);
			if(methSigCmp == 0) {
				return instr1.compareTo(instr2);
			}
			else return methSigCmp;
		}
		
	}
}
