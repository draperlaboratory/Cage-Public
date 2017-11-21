package com.draper.cage.soot;

import java.util.Iterator;
import java.util.List;

import soot.Unit;

public class CostRange
{
	private Cost lowerBound;
	private Cost upperBound;
	
	public CostRange(Cost lowerBound, Cost upperBound) {
		this.lowerBound = lowerBound;
		this.upperBound = upperBound;
	}
	
	public Cost getLowerBound() { return lowerBound; }
	
	public Cost getUpperBound() { return upperBound; }
	
	public void copyFrom(CostRange other) {
		this.lowerBound = other.lowerBound;
		this.upperBound = other.upperBound;
	}
	
	@Override
	public String toString() {
		return String.format("[%s-%s]", lowerBound, upperBound);
	}
	
	public static CostRange union(CostRange cr1, CostRange cr2) {
		Cost unionLowerBound = getLowestLowerBound(cr1, cr2);
		Cost unionUpperBound = getLargestUpperBound(cr1, cr2);
		return new CostRange(unionLowerBound, unionUpperBound);
	}
	
	private static Cost getLowestLowerBound(CostRange cr1, CostRange cr2) {
		if(Util.lt(cr1.lowerBound, cr2.lowerBound)) {
			return cr1.lowerBound;
		}
		return cr2.lowerBound;
	}

	private static Cost getLargestUpperBound(CostRange cr1, CostRange cr2) {
		if(Util.gt(cr1.upperBound, cr2.upperBound)) {
			return cr1.upperBound;
		}
		return cr2.upperBound;
	}
	
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof CostRange) {
			CostRange other = (CostRange) obj;
			return Util.eq(this.lowerBound, other.lowerBound) &&
					Util.eq(this.upperBound, other.upperBound);
		}
		return false;
	}
	
	public boolean isLargeRange() {
		return lowerBound.differsGreatly(upperBound);
	}
	
	@Override
	public int hashCode() {
		int h1 = this.lowerBound.hashCode();
		int h2 = this.upperBound.hashCode();
		return h1 * 31 + h2;
	}

	/**
	 * 
	 * @param cr
	 * @return the last element of the common statement of
	 * the lower and upper bound traces,
	 * if a common prefix exists.
	 * Otherwise, returns null.
	 * Any Unit returned from this method should be a branch.
	 */
	public static Unit getLastOfCommonPrefix(CostRange cr) {
		List<Unit> lowerTraceUnits =
			cr.getLowerBound().getTrace().getUnits();
		List<Unit> upperTraceUnits =
			cr.getUpperBound().getTrace().getUnits();
		return getLastOfCommonPrefix(lowerTraceUnits, upperTraceUnits);
	}
	
	/**
	 * 
	 * @param list1
	 * @param list2
	 * @return the last element of the common prefix of list1 and list2,
	 * if a common prefix exists.
	 * Otherwise, returns null.
	 */
	private static <T> T getLastOfCommonPrefix(List<? extends T> list1,
		List<? extends T> list2)
	{
		Iterator<? extends T> itr1 = list1.iterator();
		Iterator<? extends T> itr2 = list2.iterator();
		T lastOfCommonPrefix = null;
		while(itr1.hasNext() && itr2.hasNext()) {
			T t1 = itr1.next();
			T t2 = itr2.next();
			if(t1.equals(t2)) {
				lastOfCommonPrefix = t1;
			}
			else break;
		}
		return lastOfCommonPrefix;
	}

	public static CostRange unionNoOccur(Unit u, CostRange in1, CostRange in2) {
		if(in1.getUpperBound().getTrace().contains(u)){
			return in2;
		}
		if(in2.getUpperBound().getTrace().contains(u)){
			return in1;
		}
		return CostRange.union(in1, in2);
	}
}
