package com.draper.cage.taint;
import java.nio.charset.Charset;
import java.util.Map;

import soot.Body;
import soot.BodyTransformer;
import soot.G;
import soot.PatchingChain;
import soot.PhaseOptions;
import soot.Scene;
import soot.SootMethod;
import soot.Unit;
import soot.jimple.Stmt;
import soot.tagkit.Tag;

/**
 * 
 * @author alachyankar
 * 
 * This transformer looks for method calls that are known as tainted
 * sinks. 
 * The sinks are predetermined and are then cross-referenced with all
 * of the tagged method calls. 
 * 
 */
public class TaintedSinkTransformer extends BodyTransformer
{
	private String sinksFile;
	private MethodSet sinks;

	TaintedSinkTransformer() {
		super();
		this.sinks = new MethodSet();
	}

	/**
	 * Transform method takes a body and returns whether or not there
	 * are body lines of code that are tagged with the
	 * {@link TaintedCallTag}
	 * and checks to see if the method name is in the set of "sinks"
	 */
	@Override
	protected void internalTransform(Body b, String phaseName, Map<String, String> options) {

		this.sinksFile = PhaseOptions.getString(options, "sinks-file");

		if(!this.sinksFile.isEmpty()) {

			this.sinks.fromFile(sinksFile);

			PatchingChain<Unit> units = b.getUnits();
			for (Unit u : units) {
				if(u.hasTag("isTaintedCall")) {
					Stmt s = (Stmt) u;
					if(s.containsInvokeExpr()){
						SootMethod invokeLiteral = s.getInvokeExpr().getMethod();

						// If the original call is made
						if (this.sinks.contains(invokeLiteral)) {
							G.v().out.println("Tainted Sink found! Method: " + b.getMethod().getName() + 
									" with call for: " + invokeLiteral);
						}

						// Get the string describing dispatched methods, built with IflowAnalysis.callsToString
						Tag dTag = u.getTag("dispatchTag");
						String methodStr = new String(dTag.getValue(), Charset.defaultCharset());
						String[] dynamicDispatch = methodStr.split(";");
						//Check for empty string here, split always returns a non empty array
						if(!methodStr.isEmpty()) {
							for(int i = 0; i < dynamicDispatch.length; i++) {
								String mS = dynamicDispatch[i];
								SootMethod mI = Scene.v().getMethod(mS);
								if (this.sinks.contains(mI)){
									G.v().out.println("Tainted Sink found! Method: " + b.getMethod().getName() + 
											" with dynamic dispatch call for: " + mI + " -> " + dynamicDispatch[i]);
								}
							}

						}
					}
				}
			}
		} else {
			// G.v().out.println("Sink-file not provided. Cannot search for tainted sink calls!");
		}
	}
}
