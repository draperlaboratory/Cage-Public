package com.draper.cage.taint;
import com.draper.cage.taint.flowanalysis.IflowTransformer;

import soot.G;
import soot.Main;
import soot.PackManager;
import soot.Transform;
import soot.options.Options;


public class IflowMain
{
	/** The Soot plugin for information flow analysis.
	 * Registers an instance of IflowTransformer to the wjtp pass,
	 * registers the various options for the transform,
	 * and invokes the soot main method. 
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		G.v().out.println("Iflow Analysis.");

		//---- Create and add the Iflow Analysis transform to the wjtp pack ------

		Transform iflowT = new Transform("wjtp.iflow", new IflowTransformer());
		iflowT.setDeclaredOptions(
				"enabled summaries-file entry-method handle-exceptions cg-dot sinks-file sources-file "
				+ "graph-output graph-implicit-taint graph-sinks-only graph-assumption");

		PackManager.v().getPack("wjtp").add(iflowT);

		//----- Create and add the BranchTransform transform to the jtp pack -----

		Transform branchTransform = new Transform("jtp.btrans", new BranchTransformer());
		branchTransform.setDeclaredOptions("enabled switch-indices");
		PackManager.v().getPack("jtp").add(branchTransform);

		//----- Create and add the TaintedSinkTransformer to the jtp pack -----

		Transform taintedSinkTransform = new Transform("jtp.tstrans", new TaintedSinkTransformer());
		taintedSinkTransform.setDeclaredOptions("enabled sinks-file");
		PackManager.v().getPack("jtp").add(taintedSinkTransform);

		//----- Set various other options for the Iflow Analysis ----------------- 
		Options.v().set_whole_program(true);
		// Emit bytecode
		Options.v().set_output_format(Options.output_format_J);

		//Call Soot's main function
		Main.main(args);
	}
}
