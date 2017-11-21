package com.draper.cage.soot;

import java.util.List;
import java.util.LinkedList;
import java.util.Collections;
import java.util.Iterator;

import soot.Unit;

public class Trace
{
	private final List<Unit> units;
	
	public Trace(List<Unit> units) {
		this.units = units;
	}
	
	public static Trace createSingletonTrace(Unit unit) {
		return new Trace(Collections.singletonList(unit));
	}
	
	private static Trace emptyTrace = createEmptyTrace();
	
	private static Trace createEmptyTrace() {
		List<Unit> emptyList = Collections.emptyList();
		return new Trace(emptyList);
	}
	
	public boolean isEmpty() { return this.units.isEmpty(); }
	
	public static Trace getEmptyTrace() { return emptyTrace; }
	
	public List<Unit> getUnits() { return units; }
	
	/**
	 * This method does not modify any of its inputs
	 * @param trace1
	 * @param trace2
	 * @return a new trace containing the result of appending
	 * <code>trace1.getUnits()</code> with <code>trace2.getUnits()</code>
	 */
	public static Trace append(Trace trace1, Trace trace2) {
		List<Unit> trace1Units = trace1.getUnits();
		List<Unit> trace2Units = trace2.getUnits();
		List<Unit> combinedUnits = append(trace1Units, trace2Units);
		return new Trace(combinedUnits);
	}
	
	/**
	 * This method does not modify any of its inputs
	 * @param list1
	 * @param list2
	 * @return a new list containing the result of appending
	 * <code>list1</code> with <code>list2</code>
	 */
	private static <T> List<T> append(List<? extends T> list1,
		List<? extends T> list2)
	{
		List<T> combinedElems = duplicateList(list1);
		combinedElems.addAll(list2);
		return combinedElems;
	}
	
	private static <T> List<T> duplicateList(
		java.util.Collection<? extends T> collec)
	{
		LinkedList<T> duplicateList = new LinkedList<T>();
		duplicateList.addAll(collec);
		return duplicateList;
	}
	
	public boolean containsLoop() {
		java.util.Set<Unit> visited = new java.util.HashSet<>();
		for(Unit unit : units) {
			if(!visited.add(unit)) // if unit is already in visited
				return true;
		}
		return false;
	}
	
	@Override
	public String toString() {
		if(this.isEmpty()) {
			return "[]";
		}
		StringBuilder sb = new StringBuilder();
		sb.append("[\n");
		String tab = "  ";
		Iterator<?> itr = units.iterator();
		while(itr.hasNext()) {
			sb.append(tab).append(itr.next());
			if(itr.hasNext()) {
				sb.append(',');
			}
			sb.append('\n');
		}
		sb.append(']');
		return sb.toString();
	}

	public boolean isPrefixOf(Trace trace) {
		Iterator<Unit> itThis = this.units.iterator();
		Iterator<Unit> itThat = trace.units.iterator();
		while(itThis.hasNext()){
			if(!itThat.hasNext()){
				return false;
			}
			Unit uThis = itThis.next();
			Unit uThat = itThat.next();
			if(! uThis.equals(uThat) ){
				return false;
			}
		}
		return true;
	}

	public boolean contains(Unit u) {
		return this.units.contains(u);
	}
}
