package com.draper.cage.taint.flowanalysis;
import soot.Context;
import soot.Unit;

/**
 * A CallingContextUnit placed beneath a method call context
 * @author cdr1454
 *
 */
public class ContextCall implements CallingContextUnit {

	private Context call;
	private CallingContextUnit body;
	
	/**
	 * @param call
	 * @param body
	 */
	public ContextCall(Context call, CallingContextUnit body) {
		this.call = call;
		this.body = body;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.body == null) ? 0 : this.body.hashCode());
		result = prime * result + ((this.call == null) ? 0 : this.call.hashCode());
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof ContextCall))
			return false;
		ContextCall other = (ContextCall) obj;
		if (this.body == null) {
			if (other.body != null)
				return false;
		} else if (!this.body.equals(other.body))
			return false;
		if (this.call == null) {
			if (other.call != null)
				return false;
		} else if (!this.call.equals(other.call))
			return false;
		return true;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return this.call.toString() + " -> \n	" + this.body;
	}

	@Override
	public Unit getUnit() {
		return this.body.getUnit();
	}
	
	@Override
	public Context getContext() {
		return this.call;
	}

	@Override
	public CallingContextUnit getBody() {
		return this.body;
	}

	
	
}
