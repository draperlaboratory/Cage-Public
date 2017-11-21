package com.draper.cage.taint.json;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.draper.cage.taint.flowanalysis.ArgOrRetOrThis;
import com.draper.cage.taint.flowanalysis.Summaries;
import com.draper.cage.taint.flowanalysis.TaintAssumption;
import com.draper.cage.taint.flowanalysis.TaintStateVisitor;
import com.draper.cage.taint.flowanalysis.TaintSummary;

import soot.SootClass;
import soot.SootMethod;

/**
 * Encodes a TaintSummary into a JSON summary
 * Purpose:
 * To take a taint analysis summary and encode the resulting information
 * into a JSON object for printing or some sort of manipulation.
 */
public class JSONEncoder {

	/**
	 * Converts input summaries to a JSON object.
	 * Takes in Summaries s and parses through the summary,
	 * putting each method summary into a map sorted by the
	 * declaring class at the time. 
	 * @param s
	 * @return the JSON representation of summaries s
	 */
	@SuppressWarnings("unchecked")
	public static JSONObject summariesToJSON(Summaries s) {
		Map<SootClass, Map<SootMethod, TaintSummary>> byClass = new HashMap<>();

		//Populate byClass
		for (Map.Entry<SootMethod, TaintSummary> e : s.entrySet()) {
			SootClass mClass = e.getKey().getDeclaringClass();
			if (byClass.containsKey(mClass)) {
				byClass.get(mClass).put(e.getKey(), e.getValue());
			} else {
				Map<SootMethod, TaintSummary> cMap = new HashMap<>();
				cMap.put(e.getKey(), e.getValue());
				byClass.put(mClass, cMap);
			}
		}
		//Build the JSON class summaries
		JSONArray classSummaries = new JSONArray();

		for (Map.Entry<SootClass, Map<SootMethod,TaintSummary>> p : byClass.entrySet()) {
			classSummaries.add(JSONEncoder.classSummaryToJSON(p.getKey(), p.getValue()));
		}

		JSONObject out = new JSONObject();
		out.put("summaries", classSummaries);
		return out;
	}

	/**
	 * Utility method utilized by summariesToJSON to take a set of
	 * methods that are mapped to a particular class.
	 * @param cl
	 * @param cSumms
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static JSONObject classSummaryToJSON(SootClass cl, Map<SootMethod, TaintSummary> cSumms) {
		JSONObject out = new JSONObject();
		out.put("class", cl.getName());
		JSONArray jsonSumms = new JSONArray();
		for (Map.Entry<SootMethod, TaintSummary> e : cSumms.entrySet()) {
			jsonSumms.add(JSONEncoder.taintToJSON(e.getKey(), e.getValue()));
		}
		out.put("methods", jsonSumms);
		return out;
	}

	@SuppressWarnings("unchecked")
	public static JSONObject taintToJSON(SootMethod m, TaintSummary t) {
		JSONObject mSummary = new JSONObject();
		mSummary.put("name", m.getName());
		mSummary.put("descriptor", m.getBytecodeSignature());
		//We still use argCount for now, as a convenience
		// before moving to string type descriptors
		mSummary.put("argCount", m.getParameterCount());
		mSummary.put("static", m.isStatic());
		JSONArray tModifies = modifiesToJSON(t);
		mSummary.put("modifies", tModifies);
		JSONObject tTaint = taintMapToJSON(t);
		mSummary.put("taint", tTaint);
		return mSummary;
	}

	/**
	 * Takes out a modifies summary and returns the "modifies" segment
	 * of the ending taint summary for the JSON object
	 * @param t
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static JSONArray modifiesToJSON(TaintSummary t) {
		JSONArray mods = new JSONArray();
		for (ArgOrRetOrThis a : t.getModified()){
			mods.add(a.toString());
		}
		return mods;
	}

	/**
	 * takes the summary taint object and parses out the taint
	 * specific aspects of the summary like unconditional taint,
	 * mustTaint, taints, etc. 
	 * @param t
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static JSONObject taintMapToJSON(TaintSummary t) {
		JSONObject taintOut = new JSONObject();
		JSONArray mustTaint = new JSONArray();
		for (ArgOrRetOrThis a : t.getUncondTaint().keySet()) {
			mustTaint.add(a.toString());
		}
		taintOut.put("mustTaint", mustTaint);
		JSONArray taintAssoc = new JSONArray();
		taintOut.put("taintAssoc", taintAssoc);
		TaintStateVisitor<ArgOrRetOrThis> addAssoc =
				new TaintStateVisitor<ArgOrRetOrThis>()
		{
			@Override
			public void visit(TaintAssumption h, Set<ArgOrRetOrThis> tainted) {
				if (h.isPresent()) {
					JSONObject hAssoc = new JSONObject();
					JSONArray hTaints = new JSONArray();
					for (ArgOrRetOrThis a : tainted) {
						hTaints.add(a.toString());
					}
					hAssoc.put("pos", h.get().toString());
					hAssoc.put("taints", hTaints);
					taintAssoc.add(hAssoc);
				}
			}
		};
		t.acceptTaintVisitor(addAssoc);
		return taintOut;
	}

}
