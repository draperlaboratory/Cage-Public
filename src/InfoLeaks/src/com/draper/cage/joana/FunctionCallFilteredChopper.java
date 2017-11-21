package com.draper.cage.joana;

import edu.kit.joana.ifc.sdg.graph.SDGNode;
import edu.kit.joana.ifc.sdg.graph.SDGNode.Operation;

public class FunctionCallFilteredChopper extends FilteredChopper {
	
	public FunctionCallFilteredChopper(ProgramChopper innerChopper) {
		super(innerChopper);
	}

	@Override
	public boolean filter(SDGNode node) {
		Operation operation = node.getOperation();
		switch(operation) {
			case ENTRY: return true;
			// case EXIT: return true;
			case CALL: return true;
			default: return false;
		}
	}
}
