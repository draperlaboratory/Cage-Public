package com.draper.cage.taint.flowanalysis;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.draper.cage.taint.DispatchTag;
import com.draper.cage.taint.MethodSet;
import com.draper.cage.taint.TaintedBranchTag;
import com.draper.cage.taint.TaintedCallTag;
import com.draper.cage.taint.TaintedSinkTransformer;
import com.draper.cage.taint.json.JSONEncoder;

import soot.G;
import soot.PointsToAnalysis;
import soot.SootMethod;
import soot.Unit;
import soot.jimple.Stmt;
import soot.jimple.toolkits.annotation.purity.SootMethodFilter;
import soot.jimple.toolkits.callgraph.CallGraph;
import soot.jimple.toolkits.callgraph.Edge;

/**
 * IflowAnalysis carries out the inter-procedural taint analysis.
 * We use the call graph and the entry point to recursively create
 * a stack of methods to be analyzed, filtering out elements using
 * `filter`, an instance of
 * {@link soot.jimple.toolkits.annotation.purity.SootMethodFilter}.
 * @author cdr1454
 */
public class IflowAnalysis
{
	private Collection<SootMethod> heads;
	/**
	 * contains hand written or generated summaries of taint/runtime for
	 * particular methods that are being analyzed by soot
	 */
	private Summaries summaries;
	/**
	 * contains the list of methods in topological order as to what methods
	 * should be analyzed by soot and in which order. 
	 */
	private LinkedList<SootMethod> toAnalyze;
	
	/**
	 * The call-graph, as constructed by soot during the cg phase
	 */
	private CallGraph cg;
	
	/**
	 * Contains a Map of options to pass to printGraph() when creating
	 * an ExplanationGraph.
	 */
	private Map<String,String> graphOpts;
	
	/**
	 * Sinks for the taint analysis
	 */
	private MethodSet sinks;
	
	
	public IflowAnalysis(CallGraph cg,
			SootMethodFilter filter,
			Collection<SootMethod> heads,
			@SuppressWarnings("unused") PointsToAnalysis pa,
			Summaries sum,
			MethodSet sinks,
			boolean verbose,
			Map<String,String> graphOpts) { 
		this.heads = heads;
		this.summaries = sum;
		this.cg = cg;
		this.graphOpts = graphOpts;
		this.sinks = sinks;
		//initialize the stack of methods to analyze
		this.initializeToAnalyze(cg, filter, heads);
		//Carry out the analysis
		this.doAnalysis(verbose);
	}

	/**
	 * analyzes the methods in {@link #toAnalyze} that are initialized in
	 * topological order and adds the analysis result for each method to
	 * to {@link #summaries}.
	 * @param verbose
	 */
	private void doAnalysis(boolean verbose) {
		if (verbose) {
			G.v().out.println("List of methods to analyze: " + this.toAnalyze.toString());
		}
		while (!this.toAnalyze.isEmpty()) {
			this.analyseMethod(this.toAnalyze.pop());
		}
	}

	/**
	 * Initialize the {@link #toAnalyze} work list of methods to analyze for a given application,
	 * keeping only methods reachable from {@code heads} that satisfy {@code filter}.
	 * @param cg
	 * @param filter
	 * @param heads
	 */
	protected void initializeToAnalyze(CallGraph cg, SootMethodFilter filter, Collection<SootMethod> heads) {
		this.toAnalyze = new LinkedList<SootMethod> ();
		//recursively fetch the methods called by each method
		LinkedList<SootMethod> untreated = new LinkedList<SootMethod>(heads);
		Set<SootMethod> treated          = new HashSet<SootMethod>();
		while (!untreated.isEmpty()) {
			SootMethod m = untreated.remove();
			treated.add(m);
			if (this.summaries.containsKey(m)) {
				//Do nothing: this case occurs if
				// `m` has a hand-written summary
			} else if ( !filter.want(m) ) {
				//Create the default (possibly unsound) summary for `m`
				this.summarizeUnanalysedMethod(m);
			} else {
				if (!this.toAnalyze.contains(m)) {
					this.toAnalyze.push(m);
				}
				Iterator<Edge> mCalls = cg.edgesOutOf(m);
				while (mCalls.hasNext()) {
					SootMethod tgt = mCalls.next().tgt();
					if (!treated.contains(tgt)) {
						untreated.add(tgt);
					} else {
						//Method has been treated already
						//Or the method is recursive! Create the
						// Summary just in case:
						this.summarizeUnanalysedMethod(tgt);
					}
				}
			}
		}
	}

	/**
	 * takes a {@link soot.SootMethod} object
	 * and calls {@link MethodSummary#summarizeMethod(SootMethod, Summaries)}
	 * on it if the method has not already been summarized
	 * and adds that result to {@link #summaries}
	 * @param method
	 */
	protected void analyseMethod(SootMethod method) {
		G.v().out.println("Analyzing method: " + method);
		TaintSummary lfa = MethodSummary.summarizeMethod(method, this.summaries);
		//Add the completed analysis to the summary
		this.summaries.put(method, lfa);
	}


	protected void summarizeUnanalysedMethod(SootMethod method) {
		if (this.summaries.containsKey(method)){
			//In this case the map already contains a summary of method.
		}
		else {
			G.v().out.println("Summarizing method: " + method);
			TaintSummary lfa = MethodSummary.summarizeUnanalyzedMethod(method);
			this.summaries.put(method, lfa);
		}
	}

	/**
	 * prints the results of the summarizing the methods in the analysis.
	 */
	public void showResults(){
		G.v().out.println("The result of the flow analysis:\n");
		G.v().out.println(this.summaries.toString());
	}

	/**
	 *  Print the JSON encoding of the results of the summarizing the
	 *  methods in the analysis.
	 */
	public void showResults(boolean printJson) {
		G.v().out.println("The result of the flow analysis:\n");
		if (!printJson) {
			G.v().out.println(this.summaries.toString());
		} else {
			G.v().out.println(JSONEncoder.summariesToJSON(this.summaries));
		}
	}

	/**
	 * Looks at each method's summary in the whole program summary
	 * and tags the unconditionally tainted method calls with
	 * {@link TaintedCallTag}.
	 * This serves the purpose of universally marking the line as a
	 * tainted call for the {@link TaintedSinkTransformer}
	 * to recognize. 
	 * 
	 * In addition, tag the tainted branches in a similar manner 
	 */
	public void addTaintDispatchTags() {
		Set<Entry<SootMethod, TaintSummary>> summarySet = this.summaries.entrySet();

		for (Entry<SootMethod, TaintSummary> taint : summarySet) {
			Map<CallingContextUnit,Explanation> taintedCalls = taint.getValue().resultTaintedCalls.getAllTaint();
			for(CallingContextUnit c : taintedCalls.keySet()) {
				Unit call = c.getUnit();
				G.v().out.println("Tagging tainted call " +  call + taint.getKey());
				call.addTag(new TaintedCallTag());
				Set<SootMethod> dynamicDispatch = this.findDispatchCalls((Stmt) call);
				String tagString = IflowAnalysis.callsToString(dynamicDispatch);
				call.addTag(new DispatchTag(tagString.getBytes(Charset.defaultCharset())));
			}


			Map<CallingContextUnit,Explanation> taintedBranches = taint.getValue().resultTaintedBranches.getAllTaint();
			for(CallingContextUnit c : taintedBranches.keySet()) {
				Unit call = c.getUnit();
				//G.v().out.println("Tagging tainted call " +  call);
				call.addTag(new TaintedBranchTag());
			}
		}
	}

	/**
	 * 
	 * Takes a set of SootMethod types that represent the possible
	 * dynamic dispatch calls and creates a comma-separated string
	 * of method signatures from that set.
	 * 
	 * @param dynamicDispatch the set of dispatch calls
	 * @return string representation with comma separated signatures
	 */
	private static String callsToString(Set<SootMethod> dynamicDispatch) {
		String result = "";
		for(SootMethod m : dynamicDispatch) {
			String signature = m.getSignature();
			result = result + signature + ";";
		}

		return result;
	}


	/**
	 * Finds all possible dynamic dispatch calls for a method 
	 * 
	 * @param A statement in the program.
	 * @return the set of overriding methods that are called by
	 * the statement. 
	 */
	public Set<SootMethod> findDispatchCalls(Stmt s) {
		Set<SootMethod> dispatchCalls = new HashSet<>();
		SootMethod call = s.getInvokeExpr().getMethod();
		Iterator<Edge> edges = this.cg.edgesOutOf(s);
		while(edges.hasNext()) {
			Edge e = edges.next();
			SootMethod m = e.tgt();
			// If the method names are the same, one must implement the other
			if(m.getName().equals(call.getName())) {
				dispatchCalls.add(m);
			}

		}
		return dispatchCalls;
	}

	/**
	 * Show the summary for the main method
	 */
	public void showMain() {
		G.v().out.println("MAIN METHOD SUMMARIES: ");
		for(SootMethod m : this.heads) {
			String graphAssumption = graphOpts.get("graph-assumption");
			TaintAssumption assumption = TaintAssumption.empty();
			if (!graphAssumption.equals("")) {
				if (graphAssumption.equals("this")) {
					assumption = TaintAssumption.of(ThisValue.v());
				}
				else if (graphAssumption.equals("none")) {
					// Do nothing because we already have our default unconditional value
				}
				else if (graphAssumption.substring(0,3).equals("arg")) {
					assumption = TaintAssumption.of(ArgValue.v(Integer.parseInt(graphAssumption.substring(3, 4))));
				}
				else {
					System.out.println("Invalid Graph Taint Assumption given: " + graphAssumption);
					return;
				}
			}
			ExplanationGraph outputGraph = new ExplanationGraph(assumption, m, this.summaries, this.sinks, this.graphOpts);
			outputGraph.printGraph();
			String s = "";
			s += "Method " + m + " :\n";
			
			
			s += this.summaries.get(m);
			G.v().out.println(s);
		}
	}

	/**
	 * Show the tainted calls for a particular method that involve
	 * the specific method that is in the argument.
	 * 
	 * @param methodSignature signature for the method that is being filtered. 
	 */
	public void showMethodCallsInMain(String methodSignature) {
		G.v().out.println("Printing Main Method Summaries for calls to: " + methodSignature);
		for(SootMethod m: this.heads) {
			TaintState<CallingContextUnit> taintedCalls = this.summaries.get(m).getTaintedCalls();
			TaintState<CallingContextUnit> filteredCalls = new TaintState<>();
			for (TaintAssumption a : TaintAssumption.allAssumptions(m.getParameterCount())) {
				Map<CallingContextUnit,Explanation> callSet = taintedCalls.get(a);
				for(CallingContextUnit c: callSet.keySet()) {
					SootMethod methodCall =  ((Stmt) c.getContext()).getInvokeExpr().getMethod();
					if(methodCall.getSignature().equals(methodSignature)) {
						filteredCalls.add(a, c, callSet.get(c));
					}
				}
			}
			G.v().out.print(filteredCalls);
		}
	}
}
