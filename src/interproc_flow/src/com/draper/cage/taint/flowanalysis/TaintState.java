package com.draper.cage.taint.flowanalysis;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Represents the state of "taintedness" at the current program point
 * parameterized by the type of things we are tracking taint for.
 * This particular data set contains the state of taint of a particular
 * program point.
 * It associates this program point with a set of parameters that are
 * tainted under the same assumption.
 * The class contains a field {@link #condTaint} that maps the
 * TaintAssumptions at a beginning of a method's execution and then
 * maps that particular assumption to a set of arguments that are tainted.
 * There are methods that provide access to certain parts of the dataset.
 * The main feature of this dataset is that it can merge 2 sets of
 * tainted variables for the same initial assumption.
 *
 * @param <A> the type of things we are tracking taint for
 */
public class TaintState<A> {

	/** A key `k` represents a taintedness *assumption*. `k` is mapped
	 *  to the set of A's that are tainted under the assumption `k`. 
	 *  An `empty` key is the empty assumption.
	 */
	private Map<TaintAssumption,Map<A,Explanation>> condTaint;

	public TaintState(){
		this.condTaint = new LinkedHashMap<>();
	}

	/** Create a deep copy of src
	 * 
	 * @param src
	 */
	public TaintState(TaintState<A> src) {
		this.copyFrom(src);		
	}

	/**
	 * gets the variables that are tainted under the assumption given,
	 * if there is no record for that assumption return a new empty
	 * set of arguments and save that assumption
	 * @param h
	 * @return
	 */
	public Map<A,Explanation> get(TaintAssumption h) {
		if (this.condTaint.containsKey(h)) {
			return this.condTaint.get(h);
		}
		Map<A,Explanation> fresh = new HashMap<A,Explanation>();
		this.condTaint.put(h, fresh);
		return fresh;
	}

	/**
	 * @return all variables that are tainted under the empty
	 * assumption
	 */
	public Map<A,Explanation> getUncondTaint() {
		if (this.condTaint.containsKey(TaintAssumption.empty())) {
			return this.condTaint.get(TaintAssumption.empty());
		}
		return new HashMap<>();
	}

	public Map<A,Explanation> getAllTaint() {
		Map<A,Explanation> res = new HashMap<>();
		for (Entry<TaintAssumption, Map<A, Explanation>> e : this.condTaint.entrySet()) {
			res.putAll(e.getValue());
		}
		return res;
	}

	/**
	 * gets the taint for a taint assumption of the input
	 * {@link ArgOrRetOrThis} value a.
	 * @param a
	 * @return the taint assumption of input a
	 */
	public Map<A,Explanation> getCondTaint(ArgOrRetOrThis a) {
		if (this.condTaint.containsKey(TaintAssumption.of(a))) {
			return this.condTaint.get(TaintAssumption.of(a));
		}
		return new HashMap<>();
	}

	/**
	 * takes a visitor which visits every node in the full set of
	 * argument-assumption relationships.
	 * @param v
	 */
	public void accept(TaintStateVisitor<A> v){
		for (Entry<TaintAssumption, Map<A, Explanation>> e : this.condTaint.entrySet()) {
			TaintAssumption k = e.getKey();
			Map<A,Explanation> tainted = e.getValue();
			v.visit(k, tainted.keySet());
		}
	}

	/**
	 * takes 2 "in" TaintState arguments and merges them,
	 * the resulting value is put into the third argument
	 * @param in1
	 * @param in2
	 * @param out will store the result of merging parameters
	 * in1 and in2
	 */
	public static <A> void merge(TaintState<A> in1, TaintState<A> in2,
			TaintState<A> out) {
		out.copyFrom(in1);
		//For each element in in2, merge with the existing in1 elements
		// or create a new field if it doesn't exist.
		for (Entry<TaintAssumption, Map<A, Explanation>> e : in2.condTaint.entrySet()) {
			if (out.condTaint.containsKey(e.getKey())){
				Map<A,Explanation> outSet = out.condTaint.get(e.getKey());
				outSet.putAll(e.getValue());
			} else {
				out.condTaint.put(e.getKey(), e.getValue());
			}			
		}
	}

	/**
	 * Takes a TaintState and copies each of the values from the map of
	 * the other state and recycles any previous values that were there.
	 * @param src
	 */
	public void copyFrom(TaintState<A> src) {
		this.condTaint = new LinkedHashMap<>();
		for (Entry<TaintAssumption, Map<A, Explanation>> e : src.condTaint.entrySet()) {
			this.condTaint.put(e.getKey(), new HashMap<>(e.getValue()));
		}
	}

	/**
	 * Adds the arguments or values that are tainted for a certain
	 * assumptions to the full map of assumption-argument relationships.
	 * @param h
	 * @param a
	 */
	public void add(TaintAssumption h, A a, Explanation explanation) {
		Map<A,Explanation> hTaint = this.get(h);
		// NOTE: This is a hack. Old explanations are kept in favor of new explanations.
		// This is to prevent Explanations of objects that contain themselves
		// E.g., in a for() loop, where we must increment an int:
		// i3 = i3 + 1
		// If i3 is already tainted by some other means, then we do not add this statement
		// as a new explanation.
		// The same thing is being done in addAll()
		if (!hTaint.containsKey(a)) {
			hTaint.put(a, explanation);
		}
	}

	/**
	 * Looks up the existing set S of tainted values for input
	 * assumption `h`.
	 * Then adds the assumptions in input `as` to set S.
	 * @param h
	 * @param as
	 */
	public void addAll(TaintAssumption h, Map<A,Explanation> as) {
		Map<A,Explanation> hTaint = this.get(h);
		for (A a : as.keySet()) {
			if (!hTaint.containsKey(a)) {
				hTaint.put(a, as.get(a));
			}
		}
	}

	/**
	 * Looks up the existing set S of tainted values for input
	 * TaintAssumption h.
	 * Returns true if S is empty.
	 * @param h
	 * @return
	 */
	public boolean isEmpty(TaintAssumption h) {
		return this.get(h).isEmpty();
	}

	/**
	 * maps TaintAssumption h to set of taint values `taints`
	 * in the internal map of this TaintState.
	 * @param h
	 * @param taints
	 */
	public void put(TaintAssumption h, Map<A,Explanation> taints) {
		this.condTaint.put(h, taints);
	}

	@Override
	public String toString(){
		return this.condTaint.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.condTaint == null) ? 0 : this.condTaint.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		@SuppressWarnings("unchecked")
		TaintState<A> other = (TaintState<A>) obj;
		if (this.condTaint == null) {
			if (other.condTaint != null)
				return false;
		} else if (!this.condTaint.equals(other.condTaint))
			return false;
		return true;
	}
}
