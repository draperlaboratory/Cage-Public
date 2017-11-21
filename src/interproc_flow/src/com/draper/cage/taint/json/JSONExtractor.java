package com.draper.cage.taint.json;
import java.util.Iterator;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.draper.cage.taint.RetValue;
import com.draper.cage.taint.flowanalysis.ArgOrRetOrThis;
import com.draper.cage.taint.flowanalysis.ArgValue;
import com.draper.cage.taint.flowanalysis.ExplanationHypothesis;
import com.draper.cage.taint.flowanalysis.SummarySource;
import com.draper.cage.taint.flowanalysis.TaintAssumption;
import com.draper.cage.taint.flowanalysis.TaintSummary;
import com.draper.cage.taint.flowanalysis.ThisValue;

/**
 * To take a JSON object that contains a taint summary for some set
 * of methods in a program and extracts the taint information to an
 * actual TaintSummary object. 
 * @author jqa0822
 *
 */
public class JSONExtractor
{
	/**
	 * @param jo
	 * @return which particular argument is being affected or
	 * mentioned in each part of the taintAnalysis, which is one of
	 * 3 possible values:
	 * {@link ArgValue},
	 * {@link RetValue}, or
	 * {@link ThisValue}.
	 */
	public static ArgOrRetOrThis argFromJSON(Object jo){
		String n = jo.toString();
		ArgOrRetOrThis ar;
		if (n.compareTo("ret") == 0) {
			ar = RetValue.v();
		}
		else if (n.compareTo("this") == 0) {
			ar = ThisValue.v();
		}
		else if (n.compareTo("env") == 0) {
			System.out.println("We don't know what to do with env!");
			return null;
		}
		else {
			// hack off arg from front
			n = n.substring(3);
			ar = ArgValue.v(Integer.parseInt(n));
		}
		return ar;
	}

	@SuppressWarnings("unchecked")
	public static TaintSummary taintFromJSON(JSONObject jo){
		TaintSummary ret     = new TaintSummary(SummarySource.MANUAL);
		JSONArray modifies   = (JSONArray)jo.get("modifies");
		JSONObject jTaint = (JSONObject) jo.get("taint");
		if (jTaint == null) { throw new NullPointerException(); }
		JSONArray mustTaint  = (JSONArray)jTaint.get("mustTaint");
		JSONArray taintAssoc = (JSONArray)jTaint.get("taintAssoc");

		Iterator<JSONObject> it = mustTaint.iterator();
		// parse each of mustTaint into an an ArgOrRetOrThis, add it to the set of unconditional taints
		while (it.hasNext()) {
			Object n = it.next();
			ret.addTaint(TaintAssumption.empty(), argFromJSON(n), new ExplanationHypothesis(argFromJSON(n)));
		}

		if (taintAssoc != null){
			it = taintAssoc.iterator();
			// parse each taint association and add it to the map of taint associations
			while (it.hasNext()) {
				JSONObject n = it.next();
				ArgOrRetOrThis srcPos = argFromJSON(n.get("pos"));
				if(srcPos != null){
					JSONArray tjson = (JSONArray)n.get("taints");
					Iterator<JSONObject> it2 = tjson.iterator();
					while(it2.hasNext()){
						ret.addTaint(TaintAssumption.of(srcPos),
								argFromJSON(it2.next()), new ExplanationHypothesis(argFromJSON(n)));
					}
				}else{
					System.out.println("Source of taint assoc was env.");
				}
			}
		}
		if(modifies != null){
			it = modifies.iterator();
			//Parse each modified argument and add it to the set of modified arguments
			while (it.hasNext()) {
				Object n = it.next();
				ArgOrRetOrThis art = argFromJSON(n);
				if (art != null){
					ret.addModified(art);
				}else{
					System.out.println("Skipping a null arg or ret.");
				}
			}
		}
		return ret;
	}
}
