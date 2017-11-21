package com.draper.cage.taint;
import soot.tagkit.AttributeValueException;
import soot.tagkit.Tag;

/**
 * The class of tags which determine how to specialize a tainted
 * branch.
 * Implements interface {@link soot.tagkit.Tag}.
 * A branch number is input for creating a new instance of this class.
 * A BranchTag instance marks a branch with a certain branch number
 * signifying its state as a branch node in the information flow graph.
 * Implementation:
 * This class contains one field, branchNo, that stores the branch number
 * that is given to it during the initialization of any branchTag object.
 */
public class BranchTag implements Tag {

	private byte branchNo;

	/**
	 * 
	 * @param branchNumber the branch which should be taken.
	 * Invariant: 0 <= branchNumber <= Byte.MAX_VALUE
	 */
	public BranchTag(int branchNumber) {
		assert (0 <= branchNumber && branchNumber <= Byte.MAX_VALUE);
		this.branchNo = (byte)branchNumber;
	}

	@Override
	public String getName() {
		return "takeBranch";
	}

	@Override
	public byte[] getValue() throws AttributeValueException {
		byte[] ret = new byte[1];
		ret[0] = this.branchNo;
		return ret;
	}

}
