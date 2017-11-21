package com.draper.cage.taint.flowanalysis;

public enum SummarySource {
	/**
	 * Automatically generated annotation, based on heuristics
	 */
	AUTO,
	/**
	 * An annotation that was provided manually, either by the json interface or
	 * via a sources file
	 */
	MANUAL,
	/**
	 * An annotation that was computed as the result of the taint analysis
	 */
	ANALYSIS
}
