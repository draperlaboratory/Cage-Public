package com.draper.cage.joana;

import edu.kit.joana.ifc.sdg.graph.SDGNode;
import edu.kit.joana.ifc.sdg.util.sdg.DefaultSDGNodePredicate;
import edu.kit.joana.ifc.sdg.util.sdg.SDGNodePredicate;

public class ImportantFilteredChopper extends FilteredChopper
{
	private static final SDGNodePredicate nodeFilter =
		new DefaultSDGNodePredicate();

	public ImportantFilteredChopper(ProgramChopper innerChopper) {
		super(innerChopper);
	}

	@Override
	public boolean filter(SDGNode node) {
		return isImportantNode(node);
	}

	public static boolean isImportantNode(SDGNode node) {
		return nodeFilter.isInteresting(node);
	}
}
