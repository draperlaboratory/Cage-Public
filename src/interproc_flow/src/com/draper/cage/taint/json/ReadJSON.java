package com.draper.cage.taint.json;
import java.io.FileReader;
import java.io.IOException;
import java.util.Iterator;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.*;

import com.draper.cage.taint.flowanalysis.Summaries;
import com.draper.cage.taint.flowanalysis.TaintSummary;

import java.util.List;
import java.util.Optional;

import soot.G;
import soot.Scene;
import soot.SootClass;
import soot.SootMethod;

/**
 * Read a user-generated summary file for taint analysis on a
 * particular set of classes and methods and generate a Summaries
 * object that can then be used by the IflowAnalysis
 */
public class ReadJSON
{
	private String path = null;
	private FileReader str = null;
	private JSONObject rootObj = null;

	/** Simply reads the JSON object from path into the 
	 * rootObj field global object.
	 */
	private void read(){
		try {
			this.str = new FileReader(this.path);
			JSONParser parser = new JSONParser();
			JSONObject jsonObj = (JSONObject)parser.parse(this.str);
			this.rootObj = jsonObj;
			this.str.close();
		} catch(IOException ioe) {
			System.out.println(ioe.toString());
			System.exit(1);
		} catch(ParseException pe) {
			System.out.println(pe.toString());
			System.exit(1);
		}
	}

	/** 
	 * After root object has been parsed, produce the taint
	 * summary from the json object.
	 * Get the methods that are being named in the summaries file
	 * and produce the taint summary for each method that is named
	 * in the summary file.
	 * Must do some searching for actual SootMethod object via
	 * lookup functions.
	 * If the method is decipherable and can be found, add it to the
	 * summary along with the taint from JSON.
	 * 
	 * @param className
	 * @param summaries
	 * @param root
	 */
	private static void getMethods(String className, Summaries summaries, JSONObject root){
		//Get the class from the Scene by name lookup
		SootClass c = Scene.v().getSootClass(className);
		JSONArray funcs = (JSONArray)root.get("methods");
		@SuppressWarnings("unchecked")
		Iterator<JSONObject> fi = funcs.iterator();
		while(fi.hasNext()){
			JSONObject function = fi.next();
			TaintSummary t = JSONExtractor.taintFromJSON(function);
			String fname = "";
			int argNum = -1;
			Optional<SootMethod> m = Optional.empty();
			if (function.containsKey("name")){
				fname = (String) function.get("name");
			}else{
				G.v().out.println("Error: JSON method summary provided without name field!");
			}
			if (function.containsKey("descriptor")){
				String desc = (String) function.get("descriptor");
				m = ReadJSON.getMethodByDesc(className, fname, desc);
			}
			if ( !m.isPresent() && function.containsKey("argCount")){
				//Here `c.getMethodByNameAndNumber(fname)`
				//is imprecise: the method name is not enough in general.
				// `c.getMethod(fname)` is inconvenient, since we need the whole
				// subsignature of f. We adopt an intermediate solution (hack) which
				// assigns a summary by name and number of arguments.
				G.v().out.println("Warning: Fetching method by name and argument count, may be ambiguous.");
				argNum = (int)(long) function.get("argCount");
				m = ReadJSON.getMethodByNameAndNumber(c, fname, argNum);
			}
			if (m.isPresent()) {
				summaries.put(m.get(), t);
			} else {
				G.v().out.println("Warning: method " + c.getName() + ":" + fname + " not found in Scene!");
			}
		}
	}

	/**
	 * for the cases in which a bytecode method descriptor is used
	 * to identify the method in the JSON object, use this method
	 * to find the correct method signature from the class.
	 * @param cname
	 * @param fname
	 * @param desc
	 * @return Optional.empty() if no such method exists, Option.of(method) otherwise
	 */
	public static Optional<SootMethod> getMethodByDesc(String cname, String fname, String desc){
		String[] split = desc.split("\\)");
		String callString = split[0] + ")";
		String retType = split[1];
		SootClass c = Scene.v().getSootClass(cname);
		List<SootMethod> methods = c.getMethods();
		String methodSig = "<" + cname + ": " + fname + callString + retType+ ">";
		for(SootMethod sm : methods){
			String asString = sm.getBytecodeSignature();
			if(asString.compareTo(methodSig) == 0){
				return Optional.of(sm);
			}
			//System.out.println(methodSig + " vs " + asString);
		}

		return Optional.empty();
	}

	/** 
	 * Returns the first method of class `c` with name `fname` and number
	 * of arguments `numArgs`. Returns `Optional.empty()` if there are no such methods.
	 * For the cases in which the method is described by class, function
	 * name, and number of arguments, utilize this method to find the
	 * correct signature according to soot's lookup methods.
	 * @param c
	 * @param fname
	 * @param numArgs
	 * @return
	 */
	private static Optional<SootMethod> getMethodByNameAndNumber(SootClass c, String fname, int numArgs) {
		Iterator<SootMethod> it = c.methodIterator();
		while (it.hasNext()) {
			SootMethod m = it.next();
			if (fname.equals(m.getName()) && (numArgs == m.getParameterCount())) {
				return Optional.of(m);
			}
		}
		return Optional.empty();
	}

	private void addTaintSummary(Summaries summaries) {
		JSONArray annots = (JSONArray)this.rootObj.get("summaries");
		@SuppressWarnings("unchecked")
		Iterator<JSONObject> it = annots.iterator();
		while(it.hasNext()){
			JSONObject annot = it.next();
			String cls = (String)annot.get("class");
			if (cls == null) { throw new NullPointerException(); }
			ReadJSON.getMethods(cls, summaries, annot);
		}
	}

	/**
	 * Processes the file specified in `this.path`, parsing the JSON
	 * and populating `summaries`. This is the only exposed method.
	 * 
	 * @param summaries
	 */
	public void process(Summaries summaries){
		this.read();
		this.addTaintSummary(summaries);
	}

	public ReadJSON(String p){
		this.path = p;
	}

	public static void main(String [] argv){
		String fname = argv[0];
		System.out.println("Attempting " + fname);
		ReadJSON rj = new ReadJSON(fname);
		System.out.println("Made ReadJSON...");
		Summaries s = new Summaries();
		System.out.println("Made Summaries...");
		System.out.println("Reading in " + fname);
		rj.process(s);	
	}
}
