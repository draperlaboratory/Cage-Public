package com.draper.cage.taint.flowanalysis;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;

import com.draper.cage.taint.ApplicationMethodFilter;
import com.draper.cage.taint.CallGraphToDot;
import com.draper.cage.taint.MethodSet;
import com.draper.cage.taint.json.ReadJSON;

import soot.G;
import soot.PhaseOptions;
import soot.PointsToAnalysis;
import soot.Scene;
import soot.SceneTransformer;
import soot.SootMethod;
import soot.jimple.spark.geom.geomPA.GeomPointsTo;
import soot.jimple.toolkits.annotation.purity.SootMethodFilter;
import soot.jimple.toolkits.callgraph.CallGraph;
import soot.jimple.toolkits.callgraph.Edge;
import soot.options.SparkOptions;

/**
 * The class acts as a transformer for the entire scene of the analysis
 * and performs operations on the analysis as a whole
 * while soot is looking at the code.
 * This class' internal transform method,
 * {@link #internalTransform(String, Map)},
 * initializes a series of methods,
 * attributes, and objects that are components of the analysis
 * like class {@link IflowAnalysis}.
 */
public class IflowTransformer extends SceneTransformer
{
	/**
	 * The {@link IFlowAnalysis} instance that is passed an instance of the
	 * callgraph along with a filter for application methods and the list of
	 * entrypoint units for data flow
	 */
	private IflowAnalysis    ifa;

	/**
	 * The {@link IflowOpts} instance that stores all options for a particular
	 * iflow analysis runthrough
	 */
	private IflowOpts        iopts;

	/**
	 * callgraph object representing the full call hierarchy of a method and
	 * all the method calls that they call in the body.
	 */
	private CallGraph        cg;

	private PointsToAnalysis pa;

	/**
	 * {@link Summaries} object that stores the values of the summary analysis
	 * for each soot method in the program which includes taint, runtime, etc. 
	 */
	private Summaries        sum;
	
	/**
	 * Object that stores the set of sinks passed to the transformer, namely the
	 * methods which should not be called on sources of taint.
	 */
	private MethodSet        sinks;

	/**
	 * Object that stores the set of sources passed to the transformer, namely the
	 * methods which produce output that is always tainted.
	 */
	private MethodSet        sources;

	public IflowTransformer() {
		super();
	}

	/**
	 * initializes a series of methods, attributes, and objects that are
	 * components of the analysis.
	 */
	@Override
	protected void internalTransform(String passName, Map<String, String> opts) {
		//--------------- Initialize the options -----------------------
		this.iopts = new IflowOpts(opts); 
		
		//--------------- Add the sinks --------------------------------
		this.sinks = new MethodSet();
		if(this.iopts.hasSinksFile()) {
			String sinksFile = this.iopts.getSinksFile();
			this.sinks.fromFile(sinksFile);
		}
		
		Map<String,String> graphOpts = new HashMap<>();
		//--------------- Get the graph flag ---------------------------
		String graphName = PhaseOptions.getString(opts, "graph-output");
		if (!graphName.equals("")) {
			graphOpts.put("graph-output", graphName);
		}		
		
		//------------- Get the path-to-sinks flag----------------------
		String sinksOnly = PhaseOptions.getString(opts, "graph-sinks-only");
		graphOpts.put("graph-sinks-only", sinksOnly);
		
		//------------- Get the assumption flag ------------------------
		String assumption = PhaseOptions.getString(opts, "graph-assumption");
		graphOpts.put("graph-assumption", assumption);
		
		//------------ Get the graph-implicit-taint flag----------------
		String showImplicit = PhaseOptions.getString(opts, "graph-implicit-taint");
		graphOpts.put("graph-implicit-taint", showImplicit);
		

		// Initialize the summaries:
		// Specify the summary path by passing the option "-p wjtp.iflow summaries-file:MY_SUMMARIES.json"
		this.sum = new Summaries();
		G.v().out.println(opts.toString());
		if (this.iopts.hasSummariesFile()) {
			String summariesPath = this.iopts.getSummariesFile();
			ReadJSON sumReader = new ReadJSON(summariesPath);
			sumReader.process(this.sum);
		}
		// Add the sources from the sources file to the summaries as well
		this.sources = new MethodSet();
		if (this.iopts.hasSourcesFile()){
			String sourcesFile = this.iopts.getSourcesFile();
			this.sources.fromFile(sourcesFile);
			SourceReader.addSources(this.sum, this.sources);
		}
		
		// Initialize the points-to-analysis. We use the "geometric points-to analysis" which requires
		// initializing the options, then the analysis.
		Map<String,String> sparkOptsMap = new HashMap<String, String>();
		sparkOptsMap.put("set-impl", "hash");
		SparkOptions sparkOpts = new SparkOptions(sparkOptsMap);
		this.pa = new GeomPointsTo(sparkOpts);
		//pa.solve();
		this.cg = Scene.v().getCallGraph();

		// Initialize the entry point
		SootMethod entryPoint;
		if (this.iopts.hasEntryMethod()) {
			Optional<SootMethod> oEntryPoint = this.iopts.getEntryPoint();
			if(oEntryPoint.isPresent()) {
				entryPoint = oEntryPoint.get();
			} else {
				throw new IllegalArgumentException("Could not find method "
						+ this.iopts.getEntryMethod() + " as an entry point!");
			}
		} else {
			entryPoint = Scene.v().getMainMethod();
		}

		Iterator<Edge> it = this.cg.edgesOutOf(entryPoint);
		G.v().out.println("CG Edges out of Entry Point:" + entryPoint);
		while (it.hasNext()) {
			G.v().out.println(it.next().getTgt().method().toString());
		}
		//-----------------------------------------------------------------

		//Create the filter which dictates which methods to analyze
		SootMethodFilter applicationMethodFilter = new ApplicationMethodFilter();
		//Create an iterator for the heads of the call-graph. Currently we give only the main method.
		ArrayList<SootMethod> headsArray = new ArrayList<SootMethod>();
		headsArray.add(entryPoint);

		//Create the dot representation of the call graph
		if (opts.containsKey("cg-dot")) {
			Path dotPath = Paths.get(this.iopts.getCgDotFile());
			CallGraphToDot cgDot = new CallGraphToDot(this.cg, applicationMethodFilter, headsArray);
			cgDot.drawGraph(dotPath);
			//For now exit on CG print
			System.exit(0);
		}

		//Initialize ifa, TODO: we should have a flag for verbosity
		this.ifa = new IflowAnalysis(this.cg, applicationMethodFilter, headsArray,
				this.pa, this.sum, this.sinks, true, graphOpts);
		//		this.ifa.showResults();
		this.ifa.showMain();
		//		this.ifa.showMethodCallsInMain("<fi.iki.elonen.NanoHTTPD: fi.iki.elonen.NanoHTTPD$Response serve(fi.iki.elonen.NanoHTTPD$IHTTPSession)>");
		this.ifa.addTaintDispatchTags();
	}
}
