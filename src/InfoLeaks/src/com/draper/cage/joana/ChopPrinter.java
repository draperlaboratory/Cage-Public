package com.draper.cage.joana;

import edu.kit.joana.api.sdg.SDGProgram;

import java.io.IOException;

public interface ChopPrinter
{
	void printChop(SDGProgram program, String outputFileName,
			ProgramChop chop) throws IOException;
	
	String getFileExtension();
}
