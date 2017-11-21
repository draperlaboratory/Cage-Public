package com.draper.cage.joana;

import java.util.Collection;

import edu.kit.joana.ifc.sdg.core.SecurityNode;
import edu.kit.joana.ifc.sdg.core.conc.DataConflict;
import edu.kit.joana.ifc.sdg.core.conc.OrderConflict;
import edu.kit.joana.ifc.sdg.core.violations.IBinaryViolation;
import edu.kit.joana.ifc.sdg.core.violations.IIllegalFlow;
import edu.kit.joana.ifc.sdg.core.violations.IUnaryViolation;
import edu.kit.joana.ifc.sdg.core.violations.IViolation;
import edu.kit.joana.ifc.sdg.core.violations.IViolationVisitor;
import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

public abstract class AbstractProgramChopper implements ProgramChopper
{	
	@Override
	public final ProgramChop createChop(SDGProgram program, IViolation<SecurityNode> violation) {
		ChopperVisitor visitor = new ChopperVisitor(program);
		violation.accept(visitor);
		return visitor.getChop();
	}

	@Override
	public abstract Collection<SDGNode> getChopNodes(SDGProgram program, SDGNode source,
		SDGNode sink);
	
	private class ChopperVisitor
		implements IViolationVisitor<SecurityNode>
	{
		private final SDGProgram program;

		private ProgramChop chop;

		ChopperVisitor(SDGProgram program) {
			this.program = program;
			resetChop();
		}

		private void resetChop() {
			this.chop = null;
		}

		public ProgramChop getChop() { return chop; }

		@Override
		public void visitIllegalFlow(IIllegalFlow<SecurityNode> flow) {
			resetChop();
			SecurityNode source = flow.getSource();
			SecurityNode sink = flow.getSink();
			Collection<SDGNode> nodes = getChopNodes(program, source, sink);
			chop = new ProgramChop(flow, source, sink, nodes);
		}

		@Override
		public <L> void visitBinaryViolation(IBinaryViolation<SecurityNode, L> arg0) {
			// TODO Auto-generated method stub
		}

		@Override
		public void visitDataConflict(DataConflict<SecurityNode> arg0) {
			// TODO Auto-generated method stub
		}

		@Override
		public void visitOrderConflict(OrderConflict<SecurityNode> arg0) {
			// TODO Auto-generated method stub
		}

		@Override
		public <L> void visitUnaryViolation(IUnaryViolation<SecurityNode, L> arg0) {
			// TODO Auto-generated method stub
		}
	}
}
