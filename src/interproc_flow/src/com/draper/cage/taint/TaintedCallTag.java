package com.draper.cage.taint;
import soot.tagkit.AttributeValueException;
import soot.tagkit.Tag;

public class TaintedCallTag implements Tag
{
	@Override
	public String getName() {
		return "isTaintedCall";
	}

	@Override
	public byte[] getValue() throws AttributeValueException {
		// TODO Auto-generated method stub
		return null;
	}
}
