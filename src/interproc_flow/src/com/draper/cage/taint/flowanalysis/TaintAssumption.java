package com.draper.cage.taint.flowanalysis;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

/**
 * The datatype that represents a given assumption of taintedness
 * for a hypothesis, or None, if no arguments are assumed to be tainted.
 * 
 * @author cdr1454
 *
 */
public class TaintAssumption {

	private final Optional<ArgOrRetOrThis> hyp;

	private TaintAssumption(Optional<ArgOrRetOrThis> hyp) {
		this.hyp = hyp;
	}

	/**
	 * tells is this instance of taint assumption contains any sort
	 * of assumption for argument taint.
	 * @return
	 */
	public boolean isPresent() {
		return this.hyp.isPresent(); 
	}

	/**
	 * checks to see if there are no tainted arguments assumed
	 * @return
	 */
	public boolean isEmpty() {
		return !this.isPresent();
	}

	/**
	 * gets which arguments are tainted if they are any.
	 * Returns no argument if no argument is tainted.
	 * @return
	 */
	public ArgOrRetOrThis get() {
		return this.hyp.get();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.hyp == null) ? 0 : this.hyp.hashCode());
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
		TaintAssumption other = (TaintAssumption) obj;
		if (this.hyp == null) {
			if (other.hyp != null)
				return false;
		} else if (!this.hyp.equals(other.hyp))
			return false;
		return true;
	}

	/**
	 * factory method for instantiating a taintAssumption object,
	 * initially, with no arguments tainted
	 * @return
	 */
	public static TaintAssumption empty() {
		return new TaintAssumption(Optional.empty());
	}

	/**
	 * instantiates a TaintAssumption with the same
	 * TaintAssumption of the value a 
	 * @param a
	 * @return
	 */
	public static TaintAssumption of(ArgOrRetOrThis a) {
		return new TaintAssumption(Optional.of(a));
	}

	/** 
	 * This returns all the possible assumptions for a method,
	 * where its number of arguments should equal `numArgs`.
	 * @param numArgs
	 * @return
	 */
	public static Set<TaintAssumption> allAssumptions(int numArgs) {
		Set<TaintAssumption> ret = new HashSet<>();
		for (int i = 0; i < numArgs; i ++) {
			ret.add(TaintAssumption.of(ArgValue.v(i)));
		}
		ret.add(TaintAssumption.of(ThisValue.v()));
		ret.add(TaintAssumption.empty());
		return ret;
	}

	@Override
	public String toString() {
		if (this.isPresent()) {
			return "Some " + this.hyp.get().toString();
		}
		return "None";
	}
}
