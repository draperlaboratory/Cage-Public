package com.draper.cage.soot;
import java.util.List;

import soot.G;
import soot.Main;
import soot.PackManager;
import soot.Transform;
import soot.options.Options;


public class SimpleCostMain
{
	/**
	 * The Soot plugin for information flow analysis.
	 * Registers an instance of IflowTransformer to the wjtp pass,
	 * registers the various options for the transform,
	 * and invokes the soot main method. 
	 * 
	 * @param args
	 */
	public static void main(String[] args)
	{
		G.v().out.println("SimpleCostMain Analysis");

		// Create and add the CostTransformer transform to the jtp pack

		CostTransformer costTransformer = new CostTransformer();
		Transform costTransform = new Transform("jtp.ctrans", costTransformer);
		costTransform.setDeclaredOptions("enabled diffthreshold");
		PackManager.v().getPack("jtp").add(costTransform);

		// Set various other options for the Iflow Analysis
		
		// Commented out next line because it currently causes
		// exception soot.SootResolver$SootClassNotFoundException
		// to be thrown for class javax.crypto.spec.DESKeySpec
		// Options.v().set_whole_program(true);
		
		// Emit bytecode
		Options.v().set_output_format(Options.output_format_J);

		// Call Soot's main function
		Main.main(args);
		
		List<MethodAnalysisResult> methodAnalysisResults =
			costTransformer.getMethodAnalysisResults();
		if(methodAnalysisResults.isEmpty()) {
			G.v().out.println("NO unbalanced branches found");
		}
		else {
			G.v().out.println("Methods with unbalanced branches:");
			MethodAnalysisResult.sortByLargestDiff(methodAnalysisResults);
			for(MethodAnalysisResult result : methodAnalysisResults) {
				G.v().out.println(result);
			}
		}
	}
}
