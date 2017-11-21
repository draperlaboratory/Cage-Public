package com.draper.cage.taint;
import java.util.ArrayList;
import java.util.List;

import soot.Unit;
import soot.Value;
import soot.ValueBox;
import soot.jimple.ConditionExpr;
import soot.jimple.IfStmt;
import soot.jimple.IntConstant;
import soot.jimple.LookupSwitchStmt;
import soot.jimple.SwitchStmt;
import soot.jimple.TableSwitchStmt;
import soot.jimple.internal.JEqExpr;

/**
 * This is a class with only static methods.
 * This class implements methods to fix a branch to one of its
 * possible cases.
 * The class contains static factory methods that take a unit
 * and can determine and set the values for variables that
 * are essential to the conditional.
 *  
 * @author cdr1454
 *
 */
public class BranchTransform
{
	/**
	 * Returns the ValueBox containing the expression on which the
	 * branch statement stmt depends.
	 * 
	 * @param u an instance of IfStmt or SwitchStmt
	 * @return
	 */
	public static ValueBox getConditionBox(Unit u) {
		if (u instanceof IfStmt) {
			IfStmt ifStmt = (IfStmt)u;
			return ifStmt.getConditionBox();
		}
		assert (u instanceof SwitchStmt);
		SwitchStmt switchStmt = (SwitchStmt)u;
		return switchStmt.getKeyBox();
	}

	/**
	 * Returns a *complete* list of possible concrete
	 * values for the switch statement. This includes the
	 * default case, if applicable.
	 * 
	 * @param u an instance of IfStmt, LookupSwitchStmt or TableSwitchStmt
	 * @return
	 */
	public static List<Value> getSwitchValues(Unit u) 	{
		List<Value> ret = new ArrayList<>();
		if (u instanceof IfStmt) {
			//Create the "true" and "false" ConditionExpr in Jimple:
			ConditionExpr jTrue = new JEqExpr(IntConstant.v(0), IntConstant.v(0));
			ConditionExpr jFalse = new JEqExpr(IntConstant.v(0), IntConstant.v(1));
			ret.add(jTrue);
			ret.add(jFalse);
		} else if (u instanceof LookupSwitchStmt) {

			LookupSwitchStmt lsStmt = (LookupSwitchStmt)u;
			for (Value v : lsStmt.getLookupValues()) {
				ret.add(v);
			}
		} else {
			assert (u instanceof TableSwitchStmt);
			TableSwitchStmt tsStmt = (TableSwitchStmt)u;
			for (int i = tsStmt.getLowIndex(); i <= tsStmt.getHighIndex(); i++) {
				ret.add(IntConstant.v(i));
			}
			//Add a default value
			ret.add(IntConstant.v(tsStmt.getHighIndex() + 1));
		}
		return ret;
	}

	/**
	 * Set the condition for the branch to be the
	 * constant v.
	 * 
	 * @param u an instance of IfStmt or SwitchStmt
	 * @param v
	 */
	public static void setCondition(Unit u, Value v){
		if (u instanceof IfStmt) {
			IfStmt ifStmt = (IfStmt)u;
			ifStmt.getConditionBox().setValue(v);
		} else {
			assert (u instanceof SwitchStmt);
			SwitchStmt sStmt = (SwitchStmt)u;
			sStmt.getKeyBox().setValue(v);
		}
	}
}
