package com.draper.cage.taint.flowanalysis;
import java.util.Map;
import java.util.Optional;

import com.draper.cage.taint.json.ReadJSON;

import soot.PhaseOptions;
import soot.SootMethod;

/**
 *
 * Parses and manages the options for the IflowTransform pass
 * @author cdr1454
 */
public class IflowOpts
{
	private String summariesFile;
	private boolean handleExceptions;
	private String entryMethod;
	private String cgDotFile;
	private String sourcesFile;
	private String sinksFile;

	/**
	 * Takes a map of options that are in key-value representation
	 * and stores them in various fields of this IFlowOpts instance.
	 * @param opts a map of options that are in key-value representation
	 */
	public IflowOpts(Map<String, String> opts) {
		this.summariesFile = PhaseOptions.getString(opts, "summaries-file");
		this.handleExceptions = PhaseOptions.getBoolean(opts, "handle-exceptions", true);
		this.entryMethod = PhaseOptions.getString(opts, "entry-method");
		this.cgDotFile = PhaseOptions.getString(opts, "cg-dot");
		this.sourcesFile = PhaseOptions.getString(opts, "sources-file");
		this.sinksFile = PhaseOptions.getString(opts, "sinks-file");
	}

	public static Optional<SootMethod> sootMethodOfDesc(String descriptor) {
		String className = classNameOfDesc(descriptor);
		String methodName = methodNameOfDescriptor(descriptor);
		String argDescriptor = argDescriptorOfDesc(descriptor);
		return ReadJSON.getMethodByDesc(className, methodName, argDescriptor);
	}

	private static String classNameOfDesc(String descriptor) {
		int lastPeriodIndex = descriptor.lastIndexOf('.');
		if(lastPeriodIndex == -1) {
			throw new IllegalArgumentException(
				"Expected to find '.' in descriptor: " + descriptor);
		}
		return descriptor.substring(0, lastPeriodIndex);
	}

	private static String methodNameOfDescriptor(String descriptor) {
		int lastPeriodIndex = descriptor.lastIndexOf('.');
		if(lastPeriodIndex == -1) {
			throw new IllegalArgumentException(
				"Expected to find '.' in descriptor: " + descriptor);
		}
		int openParenIndex = descriptor.lastIndexOf('(');
		if(openParenIndex == -1) {
			throw new IllegalArgumentException(
				"Expected to find '(' in descriptor: " + descriptor);
		}
		return descriptor.substring(lastPeriodIndex+1, openParenIndex);
	}

	private static String argDescriptorOfDesc(String descriptor) {
		int openParenIndex = descriptor.lastIndexOf('(');
		if(openParenIndex == -1) {
			throw new IllegalArgumentException(
				"Expected to find '(' in descriptor: " + descriptor);
		}
		return descriptor.substring(openParenIndex);
	}

	public String getSummariesFile() {
		return this.summariesFile;
	}

	public String getCgDotFile() {
		return this.cgDotFile;
	}

	public boolean hasSummariesFile() {
		return this.summariesFile != "";
	}

	public String getEntryMethod() {
		return this.entryMethod;
	}


	public Optional<SootMethod> getEntryPoint() {
		return sootMethodOfDesc(this.entryMethod);
	}

	public boolean hasEntryMethod() {
		return this.entryMethod != "";
	}

	public boolean handleExceptions() {
		return this.handleExceptions;
	}

	public boolean hasSinksFile(){
		return this.sinksFile != "";
	}
	
	public String getSinksFile() {
		return this.sinksFile;
	}
	
	public boolean hasSourcesFile() {
		return this.sourcesFile != "";
	}

	public String getSourcesFile() {
		return this.sourcesFile;
	}
}
