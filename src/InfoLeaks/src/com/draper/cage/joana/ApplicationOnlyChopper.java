package com.draper.cage.joana;

import edu.kit.joana.ifc.sdg.graph.SDGNode;

public class ApplicationOnlyChopper extends FilteredChopper {
	
	public ApplicationOnlyChopper(ProgramChopper innerChopper) {
		super(innerChopper);
	}

	@Override
	public boolean filter(SDGNode node) {
		return isApplicationNode(node);
	}
	
	public static boolean isApplicationNode(SDGNode node) {
		String clsLoader = node.getClassLoader();
		if(clsLoader != null) {
			return clsLoader.equals("Application");
		}
		else {
			String sourceFile = node.getSource();
			if(sourceFile.startsWith("java") ||
			   sourceFile.startsWith("org/apache") ||
			   sourceFile.startsWith("sun") ||
			   sourceFile.startsWith("org/w3c"))
			{
				return false;
			}
			else return true;
		}
	}
}
