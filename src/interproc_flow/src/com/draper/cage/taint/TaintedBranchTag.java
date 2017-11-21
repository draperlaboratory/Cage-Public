package com.draper.cage.taint;
import soot.tagkit.AttributeValueException;
import soot.tagkit.Tag;

/**
 * Implements the taintedness tags for branches
 * @author cdr1454
 *
 */
public class TaintedBranchTag implements Tag {

	@Override
	public String getName() {
		return "isTaintedBranch";
	}

	/**
	 * For now, a tag simply marks a branch as unconditionally tainted,
	 * so the value contains no information
	 */
	@Override
	public byte[] getValue() throws AttributeValueException {
		return null;
	}

}
