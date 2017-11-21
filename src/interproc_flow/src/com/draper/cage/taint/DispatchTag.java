package com.draper.cage.taint;
import soot.tagkit.AttributeValueException;
import soot.tagkit.Tag;

public class DispatchTag implements Tag
{	
	private byte[] dispatchMethods;

	public DispatchTag(byte[] bytes) {
		this.dispatchMethods = bytes;
	}

	@Override
	public String getName() {
		// TODO Auto-generated method stub
		return "dispatchTag";
	}

	@Override
	public byte[] getValue() throws AttributeValueException {		
		return this.dispatchMethods;
	}
}
