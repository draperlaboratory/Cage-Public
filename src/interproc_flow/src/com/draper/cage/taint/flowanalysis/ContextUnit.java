package com.draper.cage.taint.flowanalysis;
import soot.Context;
import soot.Unit;

/**
 * A unit representing the root of a CallingContextUnit
 * @author cdr1454
 *
 */
public class ContextUnit implements CallingContextUnit {
	private Unit unit;
	
	/**
	 * @param unit
	 */
	public ContextUnit(Unit unit) {
		this.unit = unit;
	}

	@Override
	public String toString() {
		return this.unit.toString() + "\n\n";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.unit == null) ? 0 : this.unit.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof ContextUnit))
			return false;
		ContextUnit other = (ContextUnit) obj;
		if (this.unit == null) {
			if (other.unit != null)
				return false;
		} else if (!this.unit.equals(other.unit))
			return false;
		return true;
	}

	@Override
	public Unit getUnit() {
		return this.unit;
	}

	@Override
	public Context getContext() {
		return this.unit;
	}

	@Override
	public CallingContextUnit getBody() {
		return this;
	}

}
