package com.draper.cage.taint.flowanalysis;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.draper.cage.taint.MethodSet;
import com.draper.cage.taint.RetValue;

import soot.Local;
import soot.Scene;
import soot.SootMethod;
import soot.Unit;
import soot.jimple.Stmt;
import soot.jimple.toolkits.callgraph.CallGraph;

/**
 * Used to print out a digraph in DOT format that
 * explains taint flow through objects in a program. The resulting graph
 * should be a DAG, with the root being the source of taint from the given
 * assumption. If there is a manually annotated taint source, then that node will
 * be highlighted elsewhere. Taint sinks will be leaves of the tree. Solid arrows indicate taint
 * flow, and dashed arrows indicate nested method calls. Node colors indicate
 * the type of taint object. Node labels show what is tainted, as well as probable cause(s)
 * of the taintedness. The weirdness of the String creation in some of the methods in this class can
 * usually be explained by the formatting requirements of the GraphViz DOT format, and
 * will usually be explained.
 * @author rqg0530
 *
 */
public class ExplanationGraph {
	
	/**
	 * TaintAssumption under which the taint flow is being explained.
	 */
	private TaintAssumption assumption;
	
	/**
	 * Method being explained by the graph.
	 */
	private SootMethod mainMethod;
	
	/**
	 * Summaries of all summarized methods.
	 */
	private Summaries summaries;
	
	/**
	 * MethodSet of all declared taint sinks.
	 */
	private MethodSet sinks;
	
	/**
	 * MethodSet of all declared taint sources.
	 */
	private MethodSet sources;
	
	/**
	 * LocalSummary of mainMethod.
	 */
	private TaintSummary mainSummary;
	
	/**
	 * Options passed to the graph at runtime
	 */
	private Map<String,String> graphOpts;
	
	/**
	 * Set of taint flow edges of the graph. Not created
	 * until createGraph() is called.
	 */
	private Map<String, Set<String>> edges = null;
	
	/**
	 * Set of method call edges of the graph. These appear as dashed
	 * edges in the graph. Not created until createGraph() is called.
	 */
	private Map<String, Set<String>> methodCallEdges = null;
	
	/**
	 * Set of labels for each node in the graph. Not created until createGraph() is called.
	 */
	private Map<String, String> nodeLabels = null;
	
	/**
	 * Map of each explanation subgraph of the mainMethod. For each method 'm' called within
	 * mainMethod, if there is eventually a call to a taint sink, create a graph for 'm' and add
	 * it to this map. Not created until createGraph() is called.
	 */
	private Map<String,ExplanationGraph> subGraphs = null;
	
	/* 
	 * These are node attributes.
	 */
	private String taintedLocalAttributes = "[fillcolor = yellow]";
	private String taintedCallAttributes = "[fillcolor = cyan]";
	private String taintedBranchAttributes = "[fillcolor = cyan]";
	private String taintedReturnAttributes = "[fillcolor = magenta]";
	private String taintedSourceAttributes = "[fillcolor = red]";
	private String taintSinkAttributes = "[fillcolor = green]";
	
	/**
	 * Using the provided parameters, creates an ExplanationGraph. this.createGraph() is called
	 * as part of the instantiation.
	 * @param assumption TaintAssumption that we're displaying information for
	 * @param mainMethod SootMethod that we're displaying information for
	 * @param summaries Summaries object that holds a TaintSummary for every method 
	 * @param sinks MethodSet of the taint sinks of the program
	 * @param graphOpts Map of graph options
	 */
	public ExplanationGraph(TaintAssumption assumption,
							SootMethod		mainMethod,
							Summaries 		summaries,
							MethodSet 		sinks,
							Map<String,String> graphOpts) {
		// Ensure that the given method was actually analyzed.
		if (summaries.get(mainMethod).getSource().equals(SummarySource.ANALYSIS)) {
			this.mainSummary = summaries.get(mainMethod);
		}
		else {
			System.out.println(mainMethod.toString() + " is not an analyzed method!");
		}
		this.assumption = assumption;
		this.mainMethod = mainMethod;
		this.summaries = summaries;
		this.sinks = sinks;
		this.graphOpts = graphOpts;
		this.createGraph();
	}
	
	
	
	/**
	 * Used to create a node label in GraphViz format.
	 * @param name name of the node
	 * @param attributes attributes of the displayed node
	 * @param tainted object that caused the node to be tainted
	 * @param reason reason that taint flows from the object to the current node
	 * @return Map<String,String> key is the name of the node in the graph, value is the actual label
	 */
	private static Map<String,String> getNodeLabel(String name, String attributes, String tainted, String reason) {
		String nodeName = removeQuotes(name);
		String nodeLabel;
		Map<String,String> retMap = new HashMap<>();
		nodeLabel = removeQuotes(attributes) + "\n";
		nodeLabel += "[label = \"Tainted: " + removeQuotes(tainted) + "\n";
		nodeLabel += "Reason: " + removeQuotes(reason) + "\"]\n";
		retMap.put(nodeName, nodeLabel);
		return retMap;
	}
	
	/**
	 * Using the set of taintedCalls under current assumption, return those CallingContextUnits that
	 * have a taint sink as their root call. 
	 * @return Set<CallingContextUnit> return Set is a Set of method call paths that all reach a taint sink.
	 */
	private Set<CallingContextUnit> pathToSink() {
		 /*
		  * We iterate through our Set of tainted method calls.
		  * Then we check to see if any of our tainted sinks are contained
		  * in the root of the CallingContextUnit. If they are, then we add the CallingContextUnit.
		  */
		Set<CallingContextUnit> taintedCallSet;
		if (assumption.equals(TaintAssumption.empty())) {
			taintedCallSet = mainSummary.getTaintedCalls().getUncondTaint().keySet();
		}
		else {
			ArgOrRetOrThis h = assumption.get();
			taintedCallSet = mainSummary.getTaintedCalls().getCondTaint(h).keySet();
		}
		/* 
		 * TODO: This call to the CallGraph is used to try and resolve problems that happen when the mainMethod
		 * is abstract. This and other references to the CallGraph may potentially cause other problems. 
		 */
		CallGraph cg = Scene.v().getCallGraph();
		Set<CallingContextUnit> retSet = new HashSet<>();
		if (taintedCallSet.isEmpty() || sinks.isEmpty()) {
			return retSet;
		}
		for (CallingContextUnit c : taintedCallSet) {
			Unit rootCall = c.getUnit();
			SootMethod methodCall = ((Stmt)rootCall).getInvokeExpr().getMethod();
			
			// This is a hack: if we can't find a key for methodCall, we try and find concrete instances for it
			// and return the first one found
			if (!summaries.containsKey(methodCall)) {
				methodCall = cg.edgesOutOf(rootCall).next().tgt();
			}	
			if (sinks.contains(methodCall)) {
				retSet.add(c);
			}		
		}
		return retSet;
	}
	
	/**
	 * Given a Set of paths to taint sinks, return the edges associated with these paths in
	 * the form of Map<String,Set<String>>, where a key maps to a set of targets.
	 * @param methodCallsToSinks this is the object returned from pathToSink().
	 * @return
	 */
	private static Map<String,Set<String>> printCallingContextEdges(Set<CallingContextUnit> methodCallsToSinks) {
		Map<String,Set<String>> retMap = new HashMap<>();
		for (CallingContextUnit c : methodCallsToSinks) {
			CallingContextUnit currentBody = c;
			while (!currentBody.getContext().equals(currentBody.getUnit())) {
				if (!retMap.containsKey(removeQuotes(currentBody.getContext().toString()))) {
					retMap.put(removeQuotes(currentBody.getContext().toString()), new HashSet<String>());
				}
				retMap.get(removeQuotes(currentBody.getContext().toString())).add(
						removeQuotes(currentBody.getBody().getContext().toString()));
				currentBody = currentBody.getBody();
			}
		}
		return retMap;
	}
	
	/**
	 * Depth-First Search to see if there exists a path from the start node to any of the
	 * sinks.
	 * @param start starting node
	 * @param sinks set of taint sinks
	 * @param visited set of visited nodes
	 * @return
	 */
	private boolean doesPathExist(String start, Set<String> sinks, Set<String> visited) {
		if (visited.contains(start)) {
			return false;
		}
		if (sinks.contains(start)) {
			return true;
		}
		if (!edges.containsKey(start)) {
			return false;
		}
		for (String target : edges.get(start)) {
			if (start.equals(target)) {
				return false;
			}
			visited.add(start);
			if (this.doesPathExist(target, sinks, visited)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Modify the given taintedLocalEdges and taintedCallEdges by iterating through the Map of taintedLocals.
	 * @param taintedLocalEdges edges whose nodes are tainted Locals
	 * @param taintedCallEdges edges whose nodes are tainted Calls
	 */
	private void createLocalEdges(Map<String,Set<String>> taintedLocalEdges,  Map<String,Set<String>> taintedCallEdges) {
		
		Map<Local,Explanation> taintedLocals;
		
		// Get the correct taintedLocals based on the assumption.
		if (assumption.equals(TaintAssumption.empty())) {
			taintedLocals = ((LocalSummary) mainSummary).getTaintedLocals().getUncondTaint();
		}
		else {
			ArgOrRetOrThis h = assumption.get();
			taintedLocals = ((LocalSummary) mainSummary).getTaintedLocals().getCondTaint(h);
		}
		
		Map<String,String> tempEdge;
		
		/*
		 * Iterate through the Map of taintedLocals, and create edges depending on the reason for the taintedness of
		 * each Local.
		 */
		for (Local l : taintedLocals.keySet()) {
			// TODO: Messy handling of the implicit taint flag I just added. This should be handled better.
			if (taintedLocals.get(l) instanceof ExplanationLocalImplicit &&
					!graphOpts.get("graph-implicit-taint").equals("true")) {
				continue;
			}
			// Get the edges from the current explanation.
			tempEdge = taintedLocals.get(l).printNodeEdge(l.toString());
			
			// Add these edges to taintedLocalEdges.
			for (String key : tempEdge.keySet()) {
				if (!taintedLocalEdges.containsKey(key)) {
					taintedLocalEdges.put(key, new HashSet<String>());
				}
				taintedLocalEdges.get(key).add(tempEdge.get(key));
			}
			
			// Add the labels of this tainted Local.
			nodeLabels.putAll(getNodeLabel(l.toString(), taintedLocalAttributes,
					l.toString(), taintedLocals.get(l).toString()));
			
			/*
			 * This handles the case where a Local is tainted because it is assigned to the tainted Return Value
			 * of a Method Call. We have to add an edge for the method call that returns a tainted value.
			 */
			if (taintedLocals.get(l) instanceof ExplanationLocalCall) {
				tempEdge = taintedLocals.get(l).printNodeEdge(l.toString());
				ExplanationLocalCall currentExplanation = (ExplanationLocalCall)taintedLocals.get(l);
				Stmt taintedCallStmt = currentExplanation.getReason();
				SootMethod assignedMethod = currentExplanation.getMethod();
				if (summaries.containsKey(assignedMethod)) {
					TaintSummary assignedSummary = summaries.get(assignedMethod);
					switch (assignedSummary.getSource()) {
					
					/*
					 * The case where the Method Call is automatically summarized.
					 */
					case AUTO:
						// Do nothing, because we don't want to display library methods.
						break;
					
					/*
					 * The case where the Method Call is manually marked as tainted.
					 */
					case MANUAL:
						nodeLabels.putAll(getNodeLabel(taintedCallStmt.toString(),
								taintedSourceAttributes, taintedCallStmt.toString(), "Manual annotation"));
						if (!taintedCallEdges.containsKey(removeQuotes(mainMethod.toString()))) {
							taintedCallEdges.put(removeQuotes(mainMethod.toString()), new HashSet<>());
						}
						taintedCallEdges.get(removeQuotes(mainMethod.toString())).add(removeQuotes(taintedCallStmt.toString()));
						break;
						
					/*
					 * The case where the Method Call returns a tainted value as a result of the analysis.
					 */
					case ANALYSIS:
						if (assignedSummary.getTaint().get(assumption).containsKey(RetValue.v())) {
							nodeLabels.putAll(getNodeLabel(taintedCallStmt.toString(),
									taintedCallAttributes, taintedCallStmt.toString(), "Tainted Return Value"));
							Set<ArgOrRetOrThis> taintedArgs = currentExplanation.getExplanation();
							TaintAssumption calledAssumption = TaintAssumption.empty();
							for (ArgOrRetOrThis arg : taintedArgs) {
								calledAssumption = TaintAssumption.of(arg);
								break;
							}
							
							/*
							 * Create a subgraph for the tainted method call.
							 */
							Set<CallingContextUnit> callsToSinks = this.pathToSink();
							Map<String,Set<String>> callingContextEdges = ExplanationGraph.printCallingContextEdges(callsToSinks);
							if (callingContextEdges.containsKey(taintedCallStmt.toString())) {
								Map<String,String> subGraphOpts = new HashMap<>(graphOpts);
								subGraphOpts.put("graph-sinks-only", "true");
								ExplanationGraph taintedRetGraph = new ExplanationGraph(calledAssumption,
										assignedMethod, summaries,  sinks, subGraphOpts);
								subGraphs.put(assignedMethod.toString(), taintedRetGraph);
								if (!methodCallEdges.containsKey(removeQuotes(taintedCallStmt.toString()))) {
									methodCallEdges.put(removeQuotes(taintedCallStmt.toString()), new HashSet<>());
								}
								methodCallEdges.get(removeQuotes(taintedCallStmt.toString())).add(removeQuotes(assignedMethod.toString()));	
							}
						}
						break;
					}
				}						
			}
		}
	}

	/**
	 * Modify the given taintedCallEdges by iterating through the Map of taintedCalls.
	 * @param taintedCallEdges edges whose nodes are tainted Calls
	 */
	private void createCallEdges(Map<String,Set<String>> taintedCallEdges) {
		
		Map<CallingContextUnit,Explanation> taintedCalls;
		// Get the correct taintedCalls based on the assumption.
		if (assumption.equals(TaintAssumption.empty())) {
			taintedCalls = mainSummary.getTaintedCalls().getUncondTaint();
		}
		else {
			ArgOrRetOrThis h = assumption.get();
			taintedCalls = mainSummary.getTaintedCalls().getCondTaint(h);
		}
		
		Map<String,String> tempEdge;
		
		// This is used later in printCallingContextEdges().
		Set<CallingContextUnit> callsToSinks = this.pathToSink();
		Map<String,Set<String>> callingContextEdges = ExplanationGraph.printCallingContextEdges(callsToSinks);
		for (CallingContextUnit c : taintedCalls.keySet()) {
			
			tempEdge = taintedCalls.get(c).printNodeEdge(c.getContext().toString());
			
			for (String key : tempEdge.keySet()) {
				if (!taintedCallEdges.containsKey(key)) {
					taintedCallEdges.put(key, new HashSet<String>());
				}
				taintedCallEdges.get(key).add(tempEdge.get(key));
			}

			/*
			 * This loop creates explanation 'subgraphs' for methods called within the current method.
			 * These subgraphs are added to the subGraphs field.
			 */
			for (String key : callingContextEdges.keySet()) {
				
				// This is the method called in c
				SootMethod calledMethod = ((Stmt)c.getContext()).getInvokeExpr().getMethod();
				
				// This is the TaintSummary of the method called in c.
				TaintSummary calledSummary;
				// TODO: Another potentially painful use of CallGraph:
				if (!summaries.containsKey(calledMethod) || calledMethod.isAbstract() || calledMethod.getDeclaringClass().isAbstract()) {
					CallGraph cg = Scene.v().getCallGraph();
					calledMethod = cg.edgesOutOf((Stmt)c.getContext()).next().tgt();
				}
				calledSummary = summaries.get(calledMethod);

				/*
				 * If calledSummary is actually a LocalSummary, then it's not part of the standard library, and we 
				 * want to see an explanation for it in our graph. In order to do that, recurse on this calledSummary,
				 * and add it to calledGraphs. Every tainted method call in a LocalSummary will at some point either 
				 * call a method from the standard library, or no method at all.
				 */
				if (calledSummary.getSource().equals(SummarySource.ANALYSIS)) {
					/* 
					 * Create new subGraphOpts because we don't want to blow up
					 * subGraphs with too much information.
					 */
					Map<String,String> subGraphOpts = new HashMap<>(graphOpts);
					subGraphOpts.put("graph-sinks-only", "true");
					if (callingContextEdges.containsKey(c.getContext().toString())) {
						if (!methodCallEdges.containsKey(key)) {
							methodCallEdges.put(key, new HashSet<String>());
						}
						// Now we add the edge to our methodCallEdges Map
						methodCallEdges.get(key).add(calledMethod.toString());
						if (((ExplanationCall) taintedCalls.get(c)).getExplanation().isEmpty()) {
							ExplanationGraph callingContextGraph = new ExplanationGraph(TaintAssumption.empty(), 
									calledMethod, summaries, sinks, subGraphOpts);
							subGraphs.put(calledMethod.toString(), callingContextGraph);
						}
						else {
							for (ArgOrRetOrThis arg : ((ExplanationCall) taintedCalls.get(c)).getExplanation()) {
								ExplanationGraph callingContextGraph = new ExplanationGraph(TaintAssumption.of(arg), 
										calledMethod, summaries, sinks, subGraphOpts);
								subGraphs.put(calledMethod.toString(), callingContextGraph);
								break;
							}	
						}
					}
				}
			}
			// Create label for the current CallingContextUnit based on whether or not the Context is a taint sink.
			if (sinks.contains(((Stmt)c.getContext()).getInvokeExpr().getMethod())) {
				nodeLabels.putAll(getNodeLabel(c.getContext().toString(), taintSinkAttributes,
						c.getContext().toString(), taintedCalls.get(c).toString()));
			}
			else {
				nodeLabels.putAll(getNodeLabel(c.getContext().toString(), taintedCallAttributes,
						c.getContext().toString(), taintedCalls.get(c).toString()));
			}
		}
		
		
	}
	
	/**
	 * Modify the given taintedCallEdges by iterating through the Map of taintedBranches.
	 * @param taintedBranchEdges edges whose nodes are tainted Branches (and the causes of Implicit Taint).
	 */
	private void createBranchEdges(Map<String,Set<String>> taintedBranchEdges) {
		
		/* 
		 * Unsure if this method works properly. Branch statements are displayed, but unsure
		 * if they're always correctly shown. A branch might be several method calls deeper than
		 * where it actually appears in the graph, since we're just checking the Unit of each
		 * CallingContextUnit
		 * TODO: Ensure this method works properly.
		 */

		Map<CallingContextUnit,Explanation> taintedBranches;
		// Get the correct taintedBranches based on the assumption
		if (assumption.equals(TaintAssumption.empty())) {
			taintedBranches = mainSummary.getTaintedBranches().getUncondTaint();
		}
		
		else {
			ArgOrRetOrThis h = assumption.get();
			taintedBranches = mainSummary.getTaintedBranches().getCondTaint(h);
		}
		
		Map<String,String> tempEdge;
		
		for (CallingContextUnit b : taintedBranches.keySet()) {
			tempEdge = taintedBranches.get(b).printNodeEdge(b.getUnit().toString());
			// Create the label
			nodeLabels.putAll(getNodeLabel(b.getUnit().toString(), taintedBranchAttributes,
					b.getUnit().toString(), taintedBranches.get(b).toString()));
			// Create the edges
			if (tempEdge.isEmpty() && assumption.equals(TaintAssumption.empty())) {
				if (!taintedBranchEdges.containsKey(mainMethod.toString())) {
					taintedBranchEdges.put(mainMethod.toString(), new HashSet<>());
				}
				taintedBranchEdges.get(mainMethod.toString()).add(removeQuotes(b.getUnit().toString()));
			}
			for (String key : tempEdge.keySet()) {
				if (!taintedBranchEdges.containsKey(key)) {
					taintedBranchEdges.put(key, new HashSet<String>());
				}
				taintedBranchEdges.get(key).add(tempEdge.get(key));
			}	
		}
	}
	
	/**
	 * Using the fields present, create edges, node labels, and subgraphs of taint flow and method calls.
	 */
	private void createGraph() {
		Map<Local,Explanation> taintedLocals;
		// Get the correct taintedLocals based on the assumption
		if (assumption.equals(TaintAssumption.empty())) {
			taintedLocals = ((LocalSummary) mainSummary).getTaintedLocals().getUncondTaint();
		}
		else {
			ArgOrRetOrThis h = assumption.get();
			taintedLocals = ((LocalSummary) mainSummary).getTaintedLocals().getCondTaint(h);
		}

		/*
		 * These are used in createLocalEdges() and createCallEdges()
		 */
		Map<String,Set<String>> taintedLocalEdges = new HashMap<>();
		Map<String,Set<String>> taintedCallEdges = new HashMap<>();
		Map<String,Set<String>> taintedBranchEdges = new HashMap<>();
		methodCallEdges = new HashMap<String,Set<String>>();
		
		// Stores the results of recursive createGraphString() calls. These are all combined at the end of the method.
		subGraphs = new HashMap<String,ExplanationGraph>();
		
		// Maps each node to its appropriate label.
		nodeLabels = new HashMap<String,String>();
		
		/* 
		 * Edges of taint flow throughout the program.
		 */
		edges = new HashMap<String,Set<String>>();
		
		Map<String,String> tempEdge;
	
		/*
		 * This for loop is mainly used to add a specific root node to the resulting graph. This root node will always be
		 * the name of the mainMethod, and will point to Locals that are tainted directly as a result of the current assumption.
		 * This root was specifically chosen so that special edges between method calls could be created, and explanations can be
		 * seamlessly shown for methods that are called within methods. 
		 * 
		 * Under the None assumption, and if path-to-sinks-only is set to false, then this root node has no edges connected to it. 
		 * This is because the arguments don't taint anything. When path-to-sinks-only is set to true, this node would not appear 
		 * because it does not cause taint flow.
		 */
		if (!assumption.equals(TaintAssumption.empty())) {
			ArgOrRetOrThis h = assumption.get();
			for (ArgOrRetOrThis arg : mainSummary.getTaint().getCondTaint(h).keySet()) {
				Map<ArgOrRetOrThis,Local> argAlias = ((LocalSummary) mainSummary).getAlias();
				if (taintedLocals.containsKey(argAlias.get(arg))) {
					String key = removeQuotes(mainMethod.toString());
					String target = removeQuotes(argAlias.get(arg).toString());
					if (!taintedCallEdges.containsKey(key)) {
						taintedCallEdges.put(key, new HashSet<String>());
					}
					taintedCallEdges.get(key).add(target);
					nodeLabels.putAll(getNodeLabel(key, taintedCallAttributes, key, mainSummary.getTaint().getCondTaint(h).get(arg).toString()));
				}
			}
		}
		
		else {
			String key = removeQuotes(mainMethod.toString());
			nodeLabels.putAll(getNodeLabel(key, taintedCallAttributes, key, "TaintAssumption: Unconditional"));
		}
		
		/*
		 * Add an edge for tainted Return statements.
		 */
		Explanation taintedReturnExplanation = mainSummary.getTaint().get(assumption).get(RetValue.v());
		if (taintedReturnExplanation != null) {
			tempEdge = taintedReturnExplanation.printNodeEdge(taintedReturnExplanation.getReason().toString());
			for (String key : tempEdge.keySet()) {
				if (!taintedLocalEdges.containsKey(key)) {
					taintedLocalEdges.put(key, new HashSet<String>());
				}
				
				taintedLocalEdges.get(key).add(tempEdge.get(key));
				
				nodeLabels.putAll(getNodeLabel(taintedReturnExplanation.getReason().toString(),
						taintedReturnAttributes, taintedReturnExplanation.getReason().toString(), "Tainted Return Value"));
			}
		}
		
		this.createLocalEdges(taintedLocalEdges, taintedCallEdges);
		edges.putAll(taintedLocalEdges);
		
		this.createCallEdges(taintedCallEdges);
		edges.putAll(taintedCallEdges);
		
		// Only show implicit taint if the flag is passed
		if (graphOpts.get("graph-implicit-taint").equals("true")) {
			this.createBranchEdges(taintedBranchEdges);
			edges.putAll(taintedBranchEdges);
		}
		
		
	}
	
	
	/**
	 * Return a Map<String, String>, with two keys: "nodeLabels", "graphEdges". The value of both
	 * of these keys is a String in DOT format. "nodeLabels" corresponds to the label assigned to
	 * each node in the resulting graph. The label of a node is the information being displayed to
	 * the user. "graphEdges" corresponds to the edges between each node in the resulting graph. The ID
	 * of each node in the graph is prefixed by the main SootMethod, so as not to overwrite or ignore 
	 * objects with identical names across different methods in a resulting graph.
	 * @param pathToSinksOnly
	 * @return
	 */
	private Map<String, String> createGraphString(boolean pathToSinksOnly) {			
		
		Map<String,String> retMap = new HashMap<String,String>();
		
		// Get a Set<String> of nodes that form a stack of method calls to taint sinks.
		Set<String> nodesToSinks = new HashSet<>();
		if (pathToSinksOnly) {
			Set<CallingContextUnit> callsToSinks = this.pathToSink();
			for (CallingContextUnit s : callsToSinks) {
				CallingContextUnit tempContext = s;
				while (!tempContext.getContext().equals(tempContext.getUnit())) {
					nodesToSinks.add(removeQuotes(tempContext.getContext().toString()));
					tempContext = tempContext.getBody();
				}
				nodesToSinks.add(removeQuotes(tempContext.getContext().toString()));
			}
		}
		
		// Same as above, but for taint sources. TODO: Does not work when sources are nested in untainted method calls.
		Set<String> nodesToSources = new HashSet<>();
		if (pathToSinksOnly) {
			sources = new MethodSet();
			Set<SootMethod> tempSet = new HashSet<>();
			for (Entry<SootMethod, TaintSummary> m : summaries.entrySet()) {
				if (m.getValue().getSource().equals(SummarySource.MANUAL)) {
					tempSet.add(m.getKey());
				}
			}
			sources.addAll(tempSet);
			Set<CallingContextUnit> callsToSources = this.pathToSink();
			for (CallingContextUnit s : callsToSources) {
				CallingContextUnit tempContext = s;
				while (!tempContext.getContext().equals(tempContext.getUnit())) {
					nodesToSources.add(removeQuotes(tempContext.getContext().toString()));
					tempContext = tempContext.getBody();
				}
				nodesToSources.add(removeQuotes(tempContext.getContext().toString()));
			}
		}

		String edgeString = "";
		String nodeString = "";
		
		// Iterate through the given taint flow edges to output a String in DOT format.
		for (Entry<String,Set<String>> edge : edges.entrySet()) {
			String key = edge.getKey();
			for (String target : edge.getValue()) {
				boolean printEdge = true;
				if (pathToSinksOnly) {
					if (!this.doesPathExist(target, nodesToSinks, new HashSet<String>()) && !this.doesPathExist(target, nodesToSources, new HashSet<String>())) {
						printEdge = false;
					}
				}
				if (printEdge) {
					edgeString += '"' + mainMethod.toString() + key + '"';
					edgeString += "->";
					edgeString += '"' + mainMethod.toString() + target + '"';
					edgeString += "\n ";
				}
			}
		}
		
		// And here we treat methodCallEdges specially, since we want to differentiate them from taint edges.
		for (Entry<String,Set<String>> edge : methodCallEdges.entrySet()) {
			String key = edge.getKey();
			for (String target : edge.getValue()) {
				boolean printEdge = true;
				if (pathToSinksOnly) {
					if (subGraphs.containsKey(target)) {
						if (!subGraphs.get(target).doesPathExist(target, nodesToSinks, new HashSet<String>())) {
							printEdge = false;
						}
					}
				}
				if (printEdge) {
				edgeString += '"' + mainMethod.toString() + key + '"';
				edgeString += "->";
				edgeString += '"' + target + target + '"';
				edgeString += "[style = dashed]";
				edgeString += "\n ";
				}
			}
		}
		
		// Iterate through the given nodes and their labels to output a String in DOT format.
		for (String nodeName : nodeLabels.keySet()) {
			boolean printNode = true;
			if (pathToSinksOnly) {
				if (!this.doesPathExist(nodeName, nodesToSinks, new HashSet<String>()) && !this.doesPathExist(nodeName, nodesToSinks, new HashSet<String>())) {
					printNode = false;
				}
			}
			if (printNode) {
				nodeString += '"' + mainMethod.toString() + nodeName + '"' + nodeLabels.get(nodeName);
			}
		}
		
		// And finally add all the subgraphs from methods called within the current method to the resulting graph Strings
		for (ExplanationGraph calledGraph : subGraphs.values()) {
			nodeString += calledGraph.getNodeLabelString(pathToSinksOnly);
			edgeString += calledGraph.getEdgeString(pathToSinksOnly);
		}
		retMap.put("nodeLabels", nodeString);
		retMap.put("graphEdges", edgeString);
		return retMap;
	}
	


	/**
	 * This method creates a DOT file that allows for easy viewing of taint
	 * flow in the current program. This method is invoked in IflowAnalysis.showMain().
	 */
	public void printGraph() {
		// Parse and set the graph options
		String graphFileName = graphOpts.get("graph-output");
		if (graphFileName.equals("")) {
			return;
		}
		
		boolean pathToSinksOnly = false;
		if (graphOpts.get("graph-sinks-only").equals("true")) {
			pathToSinksOnly = true;
		}

		// Declare the digraph
		String graphName = "digraph ExplanationGraph {";
		// This adds more vertical spacing between nodes
		String graphAttributes = "ranksep = 2 {";
		// Declare the default style for all nodes in the graph
		String nodeAttributes = "node [fillcolor = white, shape = box, style = filled]\n";
		// Get the properly formatted graph Strings from createGraphString()
		Map<String,String> graphFileContents = this.createGraphString(pathToSinksOnly);
		String nodeLabels = graphFileContents.get("nodeLabels");
		String graphEdges = graphFileContents.get("graphEdges");

		PrintWriter graphWriter;
		try {
			// This is the name of our output file
			graphWriter = new PrintWriter(graphFileName);
			System.out.println("Writing Explanation Graph to " + graphFileName);
			// Here we just write all our previously created Strings to the file.
			graphWriter.println(graphName);
			graphWriter.println(graphAttributes);
			graphWriter.println(nodeAttributes);
			graphWriter.println(nodeLabels);
			graphWriter.println("}\n");
			graphWriter.println(graphEdges);
			graphWriter.println("}");
			graphWriter.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			assert(false);
		}
	}
	
	private String getEdgeString(boolean pathToSinksOnly) {
		return this.createGraphString(pathToSinksOnly).get("graphEdges");
	}

	private String getNodeLabelString(boolean pathToSinksOnly) {
		return this.createGraphString(pathToSinksOnly).get("nodeLabels");
	}
	
	/**
	 * This method is just used to replace quotation marks with apostrophes in
	 * strings so that the DOT file is still readable.
	 * @param s
	 * @return
	 */
	private static String removeQuotes(String s) {
		String retString;
		// This replaceAll method is potentially dangerous.
		retString = s.replaceAll("\"", "'");
		return retString;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((assumption == null) ? 0 : assumption.hashCode());
		result = prime * result + ((graphOpts == null) ? 0 : graphOpts.hashCode());
		result = prime * result + ((mainMethod == null) ? 0 : mainMethod.hashCode());
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ExplanationGraph other = (ExplanationGraph) obj;
		if (assumption == null) {
			if (other.assumption != null)
				return false;
		} else if (!assumption.equals(other.assumption))
			return false;
		if (graphOpts == null) {
			if (other.graphOpts != null)
				return false;
		} else if (!graphOpts.equals(other.graphOpts))
			return false;
		if (mainMethod == null) {
			if (other.mainMethod != null)
				return false;
		} else if (!mainMethod.equals(other.mainMethod))
			return false;
		return true;
	}
	
	
}