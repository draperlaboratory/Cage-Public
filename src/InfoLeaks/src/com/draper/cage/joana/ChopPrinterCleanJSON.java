package com.draper.cage.joana;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.TreeSet;

import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class ChopPrinterCleanJSON implements ChopPrinter
{

	@Override
	public String getFileExtension() { return "json"; }

	@Override
	public void printChop(SDGProgram program, String outputFileName,
			ProgramChop chop) throws IOException
	{
		SDG sdg = program.getSDG();
		JSONObject json = jsonOfProgramChop(sdg, chop);
		FileWriter out = new FileWriter(outputFileName);
		out.write(json.toJSONString());
		out.flush();
		out.close();
	}

	@SuppressWarnings("unchecked")
	private static JSONObject jsonOfProgramChop(SDG sdg, ProgramChop chop)
	{
		SDGNode source = chop.getSource();
		SDGNode sink = chop.getSink();
		Collection<? extends SDGNode> nodesOfChop = chop.getNodes();
		int numNodes = nodesOfChop.size();
		JSONObject metadata = new JSONObject();
		metadata.put("source", createNodeJSON(sdg, source));
		metadata.put("sink", createNodeJSON(sdg, sink));
		metadata.put("numNodes", numNodes);

		JSONObject json = new JSONObject();
		json.put("metadata", metadata);
		json.put("methods", createMethodsJSON(sdg, nodesOfChop));
		return json;
	}
	
	@SuppressWarnings("unchecked")
	private static JSONObject createNodeJSON(SDG sdg, SDGNode node) {
		JSONObject json = new JSONObject();
		json.put("id", node.getId());
		json.put("file", node.getSource());
		json.put("method", node.getBytecodeMethod());
		json.put("label", node.getLabel());
		json.put("operation", node.getOperation().toString());
		json.put("type", node.getType());

		SDGNode entry = sdg.getEntry(node);
		json.put("entryMethod", entry.getBytecodeMethod());
		return json;
	}
	
	@SuppressWarnings("unchecked")
	private static JSONArray createMethodsJSON(
			SDG sdg, Collection<? extends SDGNode> chop)
	{
		Collection<SDGNode> entryMethods = getEntryMethods(sdg, chop);
		JSONArray arr = new JSONArray();
		for(SDGNode entryMethod : entryMethods) {
			arr.add(jsonOfEntryMethod(sdg, chop, entryMethod));
		}
		return arr;
	}
	
	private static Collection<SDGNode> getEntryMethods(
			SDG sdg, Collection<? extends SDGNode> nodes)
	{
		TreeSet<SDGNode> ordered = new TreeSet<SDGNode>(SDGNode.getIDComparator());
		for(SDGNode node : nodes) {
			SDGNode entryMethod = sdg.getEntry(node);
			ordered.add(entryMethod);
		}
		return ordered;
	}
	
	@SuppressWarnings("unchecked")
	private static JSONObject jsonOfEntryMethod(
			SDG sdg, Collection<? extends SDGNode> chop, SDGNode entryMethod)
	{
		JSONArray statements = new JSONArray();
		Collection<SDGNode> nodesOfMethod =
			choppedNodesOfMethod(sdg, chop, entryMethod);
		for(SDGNode node : nodesOfMethod) {
			statements.add(node.getLabel());
		}
		JSONObject json = new JSONObject();
		json.put("id", entryMethod.getId());
		json.put("file", entryMethod.getSource());
		json.put("method", entryMethod.getBytecodeMethod());
		json.put("relevantStatements", statements);
		return json;
	}
	
	private static Collection<SDGNode> choppedNodesOfMethod(
			SDG sdg, Collection<? extends SDGNode> chop, SDGNode entryMethod)
	{
		TreeSet<SDGNode> ordered = new TreeSet<SDGNode>(SDGNode.getIDComparator());
		for(SDGNode node : chop) {
			SDGNode nodeEntryMethod = sdg.getEntry(node);
			if(nodeEntryMethod.equals(entryMethod))
				ordered.add(node);
		}
		return ordered;
	}
}
