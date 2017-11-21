package com.draper.cage.soot;

import soot.jimple.AbstractJimpleValueSwitch;
import soot.jimple.AddExpr;
import soot.jimple.AndExpr;
import soot.jimple.ArrayRef;
import soot.jimple.BinopExpr;
import soot.jimple.CastExpr;
import soot.jimple.CmpExpr;
import soot.jimple.CmpgExpr;
import soot.jimple.CmplExpr;
import soot.jimple.DivExpr;
import soot.jimple.DynamicInvokeExpr;
import soot.jimple.EqExpr;
import soot.jimple.GeExpr;
import soot.jimple.GtExpr;
import soot.jimple.InstanceInvokeExpr;
import soot.jimple.InstanceOfExpr;
import soot.jimple.InterfaceInvokeExpr;
import soot.jimple.InvokeExpr;
import soot.jimple.LeExpr;
import soot.jimple.LengthExpr;
import soot.jimple.LtExpr;
import soot.jimple.MulExpr;
import soot.jimple.NeExpr;
import soot.jimple.NegExpr;
import soot.jimple.NewArrayExpr;
import soot.jimple.NewExpr;
import soot.jimple.NewMultiArrayExpr;
import soot.jimple.OrExpr;
import soot.jimple.RemExpr;
import soot.jimple.ShlExpr;
import soot.jimple.ShrExpr;
import soot.jimple.SpecialInvokeExpr;
import soot.jimple.StaticInvokeExpr;
import soot.jimple.SubExpr;
import soot.jimple.UnopExpr;
import soot.jimple.UshrExpr;
import soot.jimple.VirtualInvokeExpr;
import soot.jimple.XorExpr;
import soot.util.Switchable;

public class ExprCostSwitch extends AbstractJimpleValueSwitch
{
	private int cost = 0;
	
	public int getCost() { return cost; }

	@Override
	public void defaultCase(Object v) {
		this.cost = 0;
	}
	
	private void caseBinopExpr(BinopExpr expr, int operationCost) {
		Switchable op1 = expr.getOp1();
		op1.apply(this);
		int op1Cost = this.getCost();
		Switchable op2 = expr.getOp2();
		op2.apply(this);
		int op2Cost = this.getCost();
		this.cost = op1Cost + op2Cost + operationCost;
	}
	
	private static final int defaultBinaryOperationCost = 1;
	
	private void caseBinopExpr(BinopExpr expr) {
		caseBinopExpr(expr, defaultBinaryOperationCost);
	}
	
	@Override
	public void caseAddExpr(AddExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseAndExpr(AndExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseCmpExpr(CmpExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseCmpgExpr(CmpgExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseCmplExpr(CmplExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseDivExpr(DivExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseEqExpr(EqExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseNeExpr(NeExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseGeExpr(GeExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseGtExpr(GtExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseLeExpr(LeExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseLtExpr(LtExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseMulExpr(MulExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseOrExpr(OrExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseRemExpr(RemExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseShlExpr(ShlExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseShrExpr(ShrExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseUshrExpr(UshrExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseSubExpr(SubExpr v) {
		caseBinopExpr(v);
	}

	@Override
	public void caseXorExpr(XorExpr v) {
		caseBinopExpr(v);
	}
	
	private static int functionCallCost = 5;
	
	private void caseInvokeExpr(InvokeExpr v) {
		int totalArgsCost = 0;
		for(Switchable arg : v.getArgs()) {
			arg.apply(this);
			int argCost = this.getCost();
			totalArgsCost += argCost;
		}
		this.cost = totalArgsCost + functionCallCost;
	}
	
	private void caseInstanceInvokeExpr(InstanceInvokeExpr v) {
		Switchable qualifier = v.getBase();
		qualifier.apply(this);
		int qualifierCost = this.getCost();
		caseInvokeExpr(v);
		int argsAndCallCost = this.getCost();
		this.cost = qualifierCost + argsAndCallCost;
	}

	@Override
	public void caseInterfaceInvokeExpr(InterfaceInvokeExpr v) {
		caseInstanceInvokeExpr(v);
	}

	@Override
	public void caseSpecialInvokeExpr(SpecialInvokeExpr v) {
		caseInstanceInvokeExpr(v);
	}

	@Override
	public void caseStaticInvokeExpr(StaticInvokeExpr v) {
		caseInvokeExpr(v);
	}

	@Override
	public void caseVirtualInvokeExpr(VirtualInvokeExpr v) {
		caseInstanceInvokeExpr(v);
	}

	@Override
	public void caseDynamicInvokeExpr(DynamicInvokeExpr v) {
		int totalBootStrapArgCost = 0;
		for(Switchable bootStrapArg : v.getBootstrapArgs()) {
			bootStrapArg.apply(this);
			int bootStrapArgCost = this.getCost();
			totalBootStrapArgCost += bootStrapArgCost;
		}
		caseInvokeExpr(v);
		int argsAndCallCost = this.getCost();
		this.cost = totalBootStrapArgCost + argsAndCallCost;
	}

	@Override
	public void caseCastExpr(CastExpr v) {
		Switchable castedExpr = v.getOp();
		castedExpr.apply(this);
		int castedExprCost = this.getCost();
		this.cost = castedExprCost + 1;
	}

	@Override
	public void caseInstanceOfExpr(InstanceOfExpr v) {
		Switchable checkedExpr = v.getOp();
		checkedExpr.apply(this);
		int checkedExprCost = this.getCost();
		this.cost = checkedExprCost + 1;
	}
	
	private static final int arrayCreationCost = 1;

	@Override
	public void caseNewArrayExpr(NewArrayExpr v) {
		Switchable sizeExpr = v.getSize();
		sizeExpr.apply(this);
		int sizeExprCost = this.getCost();
		this.cost = sizeExprCost + arrayCreationCost;
	}

	@Override
	public void caseNewMultiArrayExpr(NewMultiArrayExpr v) {
		int totalSizeExprsCost = 0;
		for(Switchable sizeExpr : v.getSizes()) {
			sizeExpr.apply(this);
			int sizeExprCost = this.getCost();
			totalSizeExprsCost += sizeExprCost;
		}
		this.cost = totalSizeExprsCost + arrayCreationCost;
	}

	@Override
	public void caseNewExpr(NewExpr v) {
		this.cost = 1;
	}
	
	private void caseUnopExpr(UnopExpr v, int unaryOperationCost) {
		Switchable innerExpr = v.getOp();
		innerExpr.apply(this);
		int innerExprCost = this.getCost();
		this.cost = innerExprCost + unaryOperationCost;
	}
	
	private static int defaultUnaryOperationCost = 1;
	
	private void caseUnopExpr(UnopExpr v) {
		caseUnopExpr(v, defaultUnaryOperationCost);
	}

	@Override
	public void caseLengthExpr(LengthExpr v) {
		caseUnopExpr(v);
	}

	@Override
	public void caseNegExpr(NegExpr v) {
		caseUnopExpr(v);
	}
	
	/**
	 * Cost for array look up expression
	 */
	@Override
    public void caseArrayRef(ArrayRef v) {
		Switchable baseExpr = v.getBase();
		baseExpr.apply(this);
		int baseExprCost = this.getCost();
		Switchable indexExpr = v.getIndex();
		indexExpr.apply(this);
		int indexExprCost = this.getCost();
		int lookUpCost = 1;
		this.cost = baseExprCost + indexExprCost + lookUpCost;
    }
}
