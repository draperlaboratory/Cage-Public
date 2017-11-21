package com.draper.cage.joana;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;

import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.ifc.sdg.graph.SDGNode;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class ChopPrinterJSON implements ChopPrinter
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
		Collection<? extends SDGNode> chopNodes = chop.getNodes();
		int numNodes = chopNodes.size();
		JSONObject metadata = new JSONObject();
		metadata.put("source", createNodeJSON(sdg, source));
		metadata.put("sink", createNodeJSON(sdg, sink));
		metadata.put("numNodes", numNodes);
		
		JSONObject json = new JSONObject();
		json.put("metadata", metadata);
		json.put("nodes", createNodesJSON(sdg, chopNodes));
		return json;
	}
	
	@SuppressWarnings("unchecked")
	private static JSONArray createNodesJSON(SDG sdg,
		Collection<? extends SDGNode> nodes)
	{
		JSONArray list = new JSONArray();
		for(SDGNode node : nodes) {
			list.add(createNodeJSON(sdg, node));
		}
		return list;
	}
	
	@SuppressWarnings("unchecked")
	private static JSONObject createNodeJSON(SDG sdg, SDGNode node) {
		SDGNode entry = sdg.getEntry(node);
		JSONObject json = new JSONObject();
		json.put("id", node.getId());
		json.put("file", node.getSource());
		json.put("method", node.getBytecodeMethod());
		json.put("label", node.getLabel());
		json.put("operation", node.getOperation().toString());
		json.put("type", node.getType());
		json.put("entryMethod", entry.getBytecodeMethod());
		return json;
	}
}
