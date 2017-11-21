package com.draper.cage.taint.flowanalysis;
import java.util.HashSet;
import java.util.Set;

import com.draper.cage.taint.RetValue;

import soot.G;
import soot.Local;
import soot.Unit;
import soot.Value;
import soot.ValueBox;
import soot.jimple.AssignStmt;
import soot.jimple.BreakpointStmt;
import soot.jimple.EnterMonitorStmt;
import soot.jimple.ExitMonitorStmt;
import soot.jimple.GotoStmt;
import soot.jimple.IdentityStmt;
import soot.jimple.IfStmt;
import soot.jimple.InstanceInvokeExpr;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;
import soot.jimple.LookupSwitchStmt;
import soot.jimple.NopStmt;
import soot.jimple.ParameterRef;
import soot.jimple.RetStmt;
import soot.jimple.ReturnStmt;
import soot.jimple.ReturnVoidStmt;
import soot.jimple.StmtSwitch;
import soot.jimple.TableSwitchStmt;
import soot.jimple.ThisRef;
import soot.jimple.ThrowStmt;

/**
 * Purpose:
 * This class contains the definition for a large set of possible
 * statements that can be made during a particular method's local
 * analysis.
 * This class takes one statement and looks to see how the possible
 * taint may be changed as a result of executing that one statement.
 * There are cases of the switch class that apply to almost all of
 * the possible instructions in the instruction set thus each line
 * can be analyzed.
 * Implementation:
 * The class implements interface {@link soot.jimple.StmtSwitch}
 * which is used to define many of the methods that are necessary
 * to traverse each unit of the body of the method.
 * For each type of instruction, there is a straightforward way to
 * determine which possible statements are going to be.
 * This is both based on the summaries that are generated.
 * @author jaltidor
 *
 */
public class UnitFlowSwitch implements StmtSwitch {

	private LocalSummary output;
	private Summaries summaries;

	public UnitFlowSwitch(LocalSummary out, Summaries summaries) {
		this.output = out;
		this.summaries = summaries;
	}

	public LocalSummary getOutput() {
		return this.output;
	}
	
	/** Gets the set of locals contained in `value`.
	 * 
	 * @param value
	 * @return
	 */
	private static Set<Local> getLocals(Value value) {
		Set<Local> res = new HashSet<Local>();
		if (value instanceof Local){
			res.add((Local) value);
		} else {
			for (ValueBox vb : value.getUseBoxes()) {
				Value v = vb.getValue();
				if (v instanceof Local) {
					res.add((Local) v);
				}
			}
		}
		return res;
	}

	/**
	 * This method should be not be called, so it throws an
	 * {@link UnsupportedOperationException}.
	 * Throwing this exception informs the user that this
	 * operation should not be possible for a jimple interpretation. 
	 */
	@Override
	public void caseBreakpointStmt(BreakpointStmt stmt) {
		//This should not occur in ordinary jimple
		throw new UnsupportedOperationException("Unsupported operation " + stmt.toString());
	}

	/**
	 * Takes the set of all possible assumptions and updates the
	 * taint information in the event that the invoke statement is called. 
	 */
	@Override
	public void caseInvokeStmt(InvokeStmt stmt) {
		//G.v().out.println("Invoke statement: " + stmt.toString());
		InvokeExpr e = stmt.getInvokeExpr();
		Set<TaintAssumption> all =
				TaintAssumption.allAssumptions(this.output.getNumArgs());

		for (TaintAssumption h : all) {
			// Add explanationCall that contains the arguments of
			// 'e' and the stmt it was called in.
			Set<ArgOrRetOrThis> taintedSet = new HashSet<>();
			
			for (int i=0; i<e.getArgs().size(); i++) {
				if (this.output.isTainted(h, e.getArgs().get(i))) {
					taintedSet.add(ArgValue.v(i));
				}
			}
			if (!e.getMethod().isStatic()) {
				InstanceInvokeExpr eInstance = (InstanceInvokeExpr) e;
				if (this.output.isTainted(h, eInstance.getBase())) {
					taintedSet.add(ThisValue.v());
				}
			}
			
			this.output.addCall(h, e, stmt, this.summaries, new ExplanationCall(taintedSet, stmt));
		}
	}

	/**
	 * Takes an assign statement and tries to update the taint for an
	 * assign statement for a variable.
	 * Can include an assign with an invoke on one side.
	 * Also, updates taint based on the output of the variables and
	 * what variables are modified in this line.
	 */
	@Override
	public void caseAssignStmt(AssignStmt stmt) {
		//G.v().out.println("Assign statement: " + stmt.toString());
		Value rVal = stmt.getRightOp();
		Value lVal = stmt.getLeftOp();
		Set<TaintAssumption> all =
				TaintAssumption.allAssumptions(this.output.getNumArgs());


		for (TaintAssumption h : all) {
			//True if the rhs is an invoke statement and the return value is tainted
			boolean taintedInvoke = false;

			//Treat the case of an invoke expression.
			if (stmt.containsInvokeExpr()) {
				InvokeExpr e = stmt.getInvokeExpr();
				// Explanation contains the arguments of the invokeExpression
				Set<ArgOrRetOrThis> taintedSet = new HashSet<>();
				for (int i=0; i<e.getArgs().size(); i++) {
					if (this.output.isTainted(h, e.getArgs().get(i))) {
						taintedSet.add(ArgValue.v(i));
					}
				}
				if (!e.getMethod().isStatic()) {
					InstanceInvokeExpr eInstance = (InstanceInvokeExpr) e;
					if (this.output.isTainted(h, eInstance.getBase())) {
						taintedSet.add(ThisValue.v());
					}
				}
				taintedInvoke = this.output.addCall(h, stmt.getInvokeExpr(), stmt, this.summaries, 
						new ExplanationCall(taintedSet, stmt));
				if (taintedInvoke) {
					// Explanation contains the stmt of the invokeExpression
					this.output.addTaint(h, lVal, new ExplanationLocalCall(taintedSet, stmt));
				}
			}
			if (this.output.isTainted(h, rVal) && !taintedInvoke) {
				// Explanation contains the right Value of the assignStmt
				this.output.addTaint(h, lVal, new ExplanationLocal(getLocals(rVal), stmt));
			}
			
			else if (this.output.hasImplicitTaint(h)) {
				// Get the cond of the ExplanationImplicits for current h
				Set<Unit> taintedBranches = new HashSet<>();
				for (CallingContextUnit c : this.output.getTaintedBranches().get(h).keySet()) {
					taintedBranches.add(c.getUnit());
				}
				this.output.addTaint(h, lVal, new ExplanationLocalImplicit(taintedBranches, stmt));
			}

			this.output.addModified(lVal);
		}
	}

	/**
	 * Can handle the assignment of something to itself whether the
	 * right value of the identity expression is a parameter or even
	 * if the right value is a reference to the "this" variable.
	 */
	@Override
	public void caseIdentityStmt(IdentityStmt stmt) {
		//G.v().out.println("Identity statement: " + stmt.toString());
		Value rVal = stmt.getRightOp();
		Value lVal = stmt.getLeftOp();
		if (rVal instanceof ParameterRef) {
			ArgOrRetOrThis a = ArgValue.v((ParameterRef) rVal);
			this.output.addLocalAlias(a, (Local) lVal);
			// Explanation contains the right Value of the IdentityStmt
			this.output.addTaint(TaintAssumption.of(a), lVal, new ExplanationLocal(getLocals(rVal), stmt));
		} else if (rVal instanceof ThisRef) {
			ArgOrRetOrThis t = ThisValue.v();
			this.output.addLocalAlias(t, (Local)lVal);
			// Explanation contains the right Value of the IdentityStmt
			this.output.addTaint(TaintAssumption.of(t), lVal, new ExplanationLocal(getLocals(rVal), stmt));
		}
		return;
	}

	/**
	 * Prints a warning that the expression is one of a monitoring
	 * statement.
	 */
	@Override
	public void caseEnterMonitorStmt(EnterMonitorStmt stmt) {
		// Do nothing except print a warning
		G.v().out.println("Entering a monitored statement!");

	}

	/**
	 * Prints a warning that the monitored code is being exited
	 */
	@Override
	public void caseExitMonitorStmt(ExitMonitorStmt stmt) {
		// Do nothing except print a warning
		G.v().out.println("Entering a monitored statement!");

	}

	/**
	 * Do nothing as there is no conditionals to this statement and
	 * nothing in the program is affected
	 */
	@Override
	public void caseGotoStmt(GotoStmt stmt) {
		// Do nothing, as the branch is unconditional

	}

	/**
	 * Updates the taint based on whether or not the local summary says
	 * the conditional statement is a tainted one.
	 * If the conditional statement is tainted, then an implicit taint
	 * is added to this particular statement.
	 */
	@Override
	public void caseIfStmt(IfStmt stmt) {
		Value cond = stmt.getCondition();

		Set<TaintAssumption> all =
				TaintAssumption.allAssumptions(this.output.getNumArgs());

		for (TaintAssumption h : all) {
			if (this.output.isTainted(h, cond)) {
				// Explanation contains the ArgOrRetOrThis of the current assumption
				Set<Local> locals = getLocals(cond);
				Set<Local> taintedLocals = new HashSet<>();
				for (Local l : locals) {
					if (this.output.isTainted(h, l)) {
						taintedLocals.add(l);
					}
				}
				this.output.addImplicitTaint(h, stmt, new ExplanationBranch(taintedLocals, stmt));
			} else {
				//Do nothing
			}
		}

	}

	/**
	 * Updates the taint based on whether or not the local summary says
	 * the key of the input look up statement, returned by method
	 * {@link soot.jimple.LookupSwitchStmt#getKey()},
	 * is a tainted one.
	 * If the key is tainted, then an implicit taint is added to this
	 * particular statement.
	 */
	@Override
	public void caseLookupSwitchStmt(LookupSwitchStmt stmt) {
		Value key = stmt.getKey();

		Set<TaintAssumption> all =
				TaintAssumption.allAssumptions(this.output.getNumArgs());

		for (TaintAssumption h : all) {
			if (this.output.isTainted(h, key)) {
				this.output.addImplicitTaint(h, stmt, new ExplanationBranch((Local) key, stmt));
			} else {
				//Do nothing
			}
		}

	}

	/** Do nothing for a NopStmt statement */
	@Override
	public void caseNopStmt(NopStmt stmt) {
		// Do nothing

	}

	/**
	 * Ret statements (return from subroutine called by jsr) should
	 * not happen in jimple.
	 * Ret statements are optimized out by Soot
	 * This method does nothing since Ret statement should not
	 * happen in jimple. 
	 */
	@Override
	public void caseRetStmt(RetStmt stmt) {
		// Ret statements (return from subroutine called by jsr) are
		// optimized out by Soot and should not happen
		throw new UnsupportedOperationException();
	}

	/**
	 * For each assumption, checks if the value of the return
	 * statement is tainted and adds that taint to the summary.
	 * Also, checks to see if there is a taint associated with a
	 * return of an invoke statement.
	 */
	@Override
	public void caseReturnStmt(ReturnStmt stmt) {
		//Some code duplication from the assigment case
		//G.v().out.println("Return statement: " + stmt.toString());
		Value retVal = stmt.getOp();

		Set<TaintAssumption> all =
				TaintAssumption.allAssumptions(this.output.getNumArgs());


		for (TaintAssumption h : all) {
			//True if the return value is an invoke statement and the return value of that invocation is tainted
			boolean taintedInvoke = false;

			//Treat the case of an invoke expression.
			if (stmt.containsInvokeExpr()) {
				InvokeExpr e = stmt.getInvokeExpr();
				InstanceInvokeExpr eInstance = (InstanceInvokeExpr) e;
				Set<ArgOrRetOrThis> taintedSet = new HashSet<>();
				for (int i=0; i<e.getArgs().size(); i++) {
					if (this.output.isTainted(h, e.getArgs().get(i))) {
						taintedSet.add(ArgValue.v(i));
					}
				}
				if (this.output.isTainted(h, eInstance.getBase())) {
					taintedSet.add(ThisValue.v());
				}
				taintedInvoke = this.output.addCall(h, stmt.getInvokeExpr(), stmt, this.summaries, 
						new ExplanationCall(taintedSet, stmt));
				if (taintedInvoke) {
					// Explanation contains the stmt of the invokeExpression
					this.output.addTaint(h, RetValue.v(), new ExplanationLocalCall(taintedSet, stmt));
				}
			}
			

			if (this.output.isTainted(h, retVal) && !taintedInvoke) {
				// Explanation contains the right Value of the assignStmt
				this.output.addTaint(h, RetValue.v(), new ExplanationLocal(getLocals(retVal), stmt));
				
			}
			else if (this.output.hasImplicitTaint(h)) {
				// Get the cond of the ExplanationImplicits for current h
				Set<Unit> taintedBranches = new HashSet<>();
				for (CallingContextUnit c : this.output.getTaintedBranches().get(h).keySet()) {
					taintedBranches.add(c.getUnit());
				}
				this.output.addTaint(h, RetValue.v(), new ExplanationLocalImplicit(taintedBranches, stmt));
			}
		}
		return;
	}

	/**
	 * Do nothing for statements that return nothing/void.
	 */
	@Override
	public void caseReturnVoidStmt(ReturnVoidStmt stmt) {
		// Do nothing

	}

	/**
	 * The implementation of this method is the same as the
	 * implementation of method
	 * {@link #caseLookupSwitchStmt(LookupSwitchStmt)}.
	 * Updates the taint based on whether or not the local summary says
	 * the key of the input look up statement, returned by method
	 * {@link soot.jimple.LookupSwitchStmt#getKey()},
	 * is a tainted one.
	 * If the key is tainted, then an implicit taint is added to this
	 * particular statement.
	 */
	@Override
	public void caseTableSwitchStmt(TableSwitchStmt stmt) {
		//Code duplication from LookupSwitchStmt
		Value key = stmt.getKey();

		Set<TaintAssumption> all =
				TaintAssumption.allAssumptions(this.output.getNumArgs());

		for (TaintAssumption h : all) {
			if (this.output.isTainted(h, key)) {
				// Explanation contains the ArgOrRetOrThis of the current assumption
				this.output.addImplicitTaint(h, stmt, new ExplanationBranch((Local) key, stmt));
			} else {
				//Do nothing	
			}
		}

	}

	/**
	 * Do nothing, for now.
	 * We don't currently keep track of the information contained
	 * inside the exceptions, and the control flow is unconditional.
	 */
	@Override
	public void caseThrowStmt(ThrowStmt stmt) {
		// Do nothing, for now. We don't currently keep track of the information
		// contained inside the exceptions, and the control flow is
		// un-conditional

	}

	/**
	 * Throws an {@link java.lang.UnsupportedOperationException}.
	 * This type of instruction is not supported. 
	 */
	@Override
	public void defaultCase(Object obj) {
		throw new UnsupportedOperationException("The instruction " + obj.toString() + " is not supported!");

	}
}
