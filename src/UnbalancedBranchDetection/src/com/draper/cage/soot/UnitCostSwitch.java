package com.draper.cage.soot;

import soot.jimple.AbstractStmtSwitch;
import soot.jimple.AssignStmt;
import soot.jimple.EnterMonitorStmt;
import soot.jimple.ExitMonitorStmt;
import soot.jimple.GotoStmt;
import soot.jimple.IdentityStmt;
import soot.jimple.IfStmt;
import soot.jimple.InvokeStmt;
import soot.jimple.LookupSwitchStmt;
import soot.jimple.MonitorStmt;
import soot.jimple.RetStmt;
import soot.jimple.ReturnStmt;
import soot.jimple.ReturnVoidStmt;
import soot.jimple.SwitchStmt;
import soot.jimple.TableSwitchStmt;
import soot.jimple.ThrowStmt;
import soot.util.Switchable;

public class UnitCostSwitch extends AbstractStmtSwitch
{
	/** Computational cost of unit just analyzed */
	private int cost = 0;
	
	private final ExprCostSwitch exprCostSwitch = new ExprCostSwitch();
	
	public int getCost() { return cost; }

	@Override
    public void caseInvokeStmt(InvokeStmt stmt) {
		Switchable invokeExpr = stmt.getInvokeExpr();
		invokeExpr.apply(exprCostSwitch);
		this.cost = exprCostSwitch.getCost();
    }

    @Override
    public void caseAssignStmt(AssignStmt stmt) {
    	Switchable rightOp = stmt.getRightOp();
    	rightOp.apply(exprCostSwitch);
    	int rightOpCost = exprCostSwitch.getCost();
    	this.cost = rightOpCost;
    }

    @Override
    public void caseIfStmt(IfStmt stmt) {
    	Switchable condValue = stmt.getCondition();
    	condValue.apply(exprCostSwitch);
    	int condCost = exprCostSwitch.getCost();
    	this.cost = condCost;
    	/*
    	Switchable targetValue = stmt.getTarget();
    	targetValue.apply(this);
    	int targetCost = this.getCost();
        this.cost = condCost + targetCost;
        */
    }

    @Override
    public void caseLookupSwitchStmt(LookupSwitchStmt stmt) {
    	caseSwitchStmt(stmt);
    }
    
    private void caseSwitchStmt(SwitchStmt stmt) {
    	Switchable key = stmt.getKey();
    	key.apply(exprCostSwitch);
    	int keyCost = exprCostSwitch.getCost();
    	this.cost = keyCost;
    	/*
    	int totalTargetsCost = 0;
    	List<Unit> targets = stmt.getTargets();
    	for(Unit target : targets) {
    		target.apply(exprCostSwitch);
    		int targetCost = exprCostSwitch.getCost();
    		totalTargetsCost += targetCost;
    	}
    	this.cost = keyCost + totalTargetsCost;
    	*/
    }

    @Override
    public void caseRetStmt(RetStmt stmt) {
        throw new IllegalStateException("should not reach here");
    }

    @Override
    public void caseTableSwitchStmt(TableSwitchStmt stmt) {
    	caseSwitchStmt(stmt);
    }

    @Override
    public void caseThrowStmt(ThrowStmt stmt) {
    	Switchable op = stmt.getOp();
    	op.apply(exprCostSwitch);
    	int opCost = exprCostSwitch.getCost();
    	this.cost = opCost;
    }
    
    @Override
    public void caseIdentityStmt(IdentityStmt stmt) {
    	this.cost = 0;
    }

    @Override
	public void caseReturnStmt(ReturnStmt stmt) {
		Switchable invokeExpr = stmt.getOp();
		invokeExpr.apply(exprCostSwitch);
		this.cost = exprCostSwitch.getCost();
    }

    @Override
	public void caseReturnVoidStmt(ReturnVoidStmt stmt) {
    	this.cost = 0;
    }
    
    @Override
	public void caseGotoStmt(GotoStmt stmt) {
    	this.cost = 0;
    }
    
    @Override
    public void caseExitMonitorStmt(ExitMonitorStmt stmt) {
    	caseMonitorStmt(stmt);
    }

    @Override
    public void caseEnterMonitorStmt(EnterMonitorStmt stmt) {
    	caseMonitorStmt(stmt);
    }

    private void caseMonitorStmt(MonitorStmt stmt) {
    	Switchable op = stmt.getOp();
    	op.apply(exprCostSwitch);
    	int opCost = exprCostSwitch.getCost();
    	this.cost = opCost;
    }

	@Override
	public void defaultCase(Object obj) {
		throw new UnsupportedOperationException(
			"unexpected instruction: " + obj);
		// this.cost = 0;
	}
}
