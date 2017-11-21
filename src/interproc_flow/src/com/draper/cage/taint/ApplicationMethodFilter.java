package com.draper.cage.taint;
import soot.SootMethod;
import soot.jimple.toolkits.annotation.purity.SootMethodFilter;

/**
 * @author cdr1454
 *
 * Return the filter that eliminates all non-application methods.
 * 
 */
public class ApplicationMethodFilter implements SootMethodFilter
{
	/**
	 * Takes a method and returns whether or not that particular
	 * method call is from an application class.
	 */
	@Override
	public boolean want(SootMethod m) {
		return m.getDeclaringClass().isApplicationClass();
	}
}