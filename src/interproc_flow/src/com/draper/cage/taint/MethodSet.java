package com.draper.cage.taint;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import com.draper.cage.taint.flowanalysis.IflowOpts;

import soot.G;
import soot.SootMethod;

public class MethodSet {
	
	Set<SootMethod> methods;

	public MethodSet(){
		this.methods = new HashSet<>();
	}
	
	
	public void addAll(Collection<SootMethod> s) {
		this.methods.addAll(s);
	}
	
	public Set<SootMethod> get() {
		return this.methods;
	}
	
	public boolean isEmpty() {
		return this.methods.isEmpty();
	}
	
	public boolean contains(SootMethod m) {
		return this.methods.contains(m);
	}
	
	public void fromFile(String fileName) {

		String methodString;

		try {
			FileReader file = new FileReader(fileName);
			BufferedReader buffer = new BufferedReader(file);
			while((methodString = buffer.readLine()) != null) {
				Optional<SootMethod> oMethod = IflowOpts.sootMethodOfDesc(methodString);
				if(oMethod.isPresent()){
					this.methods.add(oMethod.get());
				} else {
					G.v().out.print("Warning: method " + methodString + " from file "
							+ fileName + " not found in application!");
				}
			}
			buffer.close();
		} catch (IOException e) {
			G.v().out.println("Error Reading File: " + fileName + " error: " + e);
		}

	}

	
}
