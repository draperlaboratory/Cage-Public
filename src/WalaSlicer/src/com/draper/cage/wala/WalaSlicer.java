package com.draper.cage.wala;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;

import com.ibm.wala.ipa.callgraph.impl.Util;
import com.ibm.wala.ipa.callgraph.AnalysisCache;
import com.ibm.wala.ipa.callgraph.AnalysisOptions;
import com.ibm.wala.ipa.callgraph.AnalysisOptions.ReflectionOptions;
import com.ibm.wala.ipa.callgraph.AnalysisScope;
import com.ibm.wala.ipa.callgraph.CGNode;
import com.ibm.wala.ipa.callgraph.CallGraph;
import com.ibm.wala.ipa.callgraph.CallGraphBuilder;
import com.ibm.wala.ipa.callgraph.CallGraphBuilderCancelException;
import com.ibm.wala.ipa.callgraph.Entrypoint;
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey;
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis;
import com.ibm.wala.ipa.cha.ClassHierarchy;
import com.ibm.wala.ipa.slicer.NormalStatement;
import com.ibm.wala.ipa.slicer.Slicer;
import com.ibm.wala.ipa.slicer.Slicer.ControlDependenceOptions;
import com.ibm.wala.ipa.slicer.Slicer.DataDependenceOptions;
import com.ibm.wala.ipa.slicer.Statement;
import com.ibm.wala.ipa.slicer.thin.ThinSlicer;
import com.ibm.wala.ssa.IR;
import com.ibm.wala.ssa.SSAInstruction;
import com.ibm.wala.types.ClassLoaderReference;
import com.ibm.wala.types.Descriptor;
import com.ibm.wala.types.MethodReference;
import com.ibm.wala.types.TypeReference;
import com.ibm.wala.util.CancelException;
import com.ibm.wala.util.NullProgressMonitor;
import com.ibm.wala.util.WalaException;
import com.ibm.wala.util.config.AnalysisScopeReader;
import com.ibm.wala.util.debug.Assertions;
import com.ibm.wala.util.strings.Atom;
import com.ibm.wala.util.strings.StringStuff;

public class WalaSlicer
{
	public static void main(String[] args) throws Exception
	{
		if(args.length < 2) {
			System.err.println("usage: java -jar WalaSlicer.jar <appJar> <main class> [exclusions file]");
			System.exit(1);
		}
		String appJar = args[0];
		String mainClass = args[1];
		File exclusionsFile = (args.length > 1) ? new File(args[2]) : null;
		doSlicing(appJar, mainClass, exclusionsFile);
		// unitTest();
	}
	
	private static void unitTest() throws Exception
	{
		String cageDir = "/Users/jaltidor/allwork/draper/stac/cage_prj/cage";
		String appJar = cageDir 
				+ "/engagement_1/stac_engagement_1_release_v1.0/Challenge_Programs/blogger/challenge_program"
				+ "/nanohttpd-javawebserver-2.2.0-SNAPSHOT-jar-with-dependencies.jar";
		String mainClass = "fi.iki.elonen.JavaWebServer";
		File exclusionsFile = new File("analysis_exclusions.txt");
		doSlicing(appJar, mainClass, exclusionsFile);
	}

	public static void doSlicing(String appJar, String mainClass, File exclusionsFile) throws
		WalaException, IOException, CallGraphBuilderCancelException, CancelException
	{
		// create an analysis scope representing the appJar as a J2SE application
		AnalysisScope scope = AnalysisScopeReader.makeJavaBinaryAnalysisScope(appJar, exclusionsFile);
		ClassHierarchy cha = ClassHierarchy.make(scope);
		
		String mainClassWalaName =
			StringStuff.deployment2CanonicalTypeString(mainClass);
		Iterable<Entrypoint> entrypoints = Util.makeMainEntrypoints(scope, cha, mainClassWalaName);
		AnalysisOptions options = new AnalysisOptions(scope, entrypoints);
		options.setReflectionOptions(ReflectionOptions.NONE);

		// build the call graph
		CallGraphBuilder<InstanceKey> cgb =
			Util.makeZeroCFABuilder(options, new AnalysisCache(),cha, scope, null, null);
		CallGraph cg = cgb.makeCallGraph(options, new NullProgressMonitor());
		PointerAnalysis<InstanceKey> pa = cgb.getPointerAnalysis();

		/*
		MethodReference mref = MethodReference.findOrCreate(
				ClassLoaderReference.Application,
				"Lfi/iki/elonen/URIVerifier",
				"verify",
				"(Ljava/lang/String;)Z");
		Set<CGNode> verifyMethodNodes = cg.getNodes(mref);
		Collection<Statement> seeds = new java.util.LinkedList<>();
		for(CGNode node : verifyMethodNodes) {
			Statement seed = getFirstNormalStatement(node);
			seeds.add(seed);
		}
		if(seeds.isEmpty()) {
			System.err.println("No seed found");
			return;
		}
		else {
			statement = seeds.iterator().next();
		}
		*/
		
		CGNode verifyMethodNode = findMethod(cg, "verify");
		
		// find seed statement
		Statement statement = getFirstNormalStatement(verifyMethodNode);	
		Collection<Statement> slice;
		
		// findMethod

		// statement = findCallTo(findMainMethod(cg), "println");

		// context-sensitive traditional slice
		// slice = Slicer.computeBackwardSlice ( statement, cg, pa );
		// dumpSlice(slice);

		// context-sensitive thin slice
		slice = Slicer.computeBackwardSlice(statement, cg, pa, DataDependenceOptions.NO_BASE_PTRS,
				ControlDependenceOptions.NONE);
		dumpSlice(slice);

		// context-insensitive slice
		ThinSlicer ts = new ThinSlicer(cg,pa);
		slice = ts.computeBackwardThinSlice ( statement );
		dumpSlice(slice);
	}
	
	public static Statement getFirstNormalStatement(CGNode n) {
		// return new NormalStatement(n, 1);
	    IR ir = n.getIR();
	    for (int i = 0; i < ir.getInstructions().length; i++) {
	      SSAInstruction s = ir.getInstructions()[i];
	      if (s.hasDef()) {
	        return new NormalStatement(n, i);
	      }
	    }
		return findCallTo(n, "verify");
	}
	
	public static CGNode findMainMethod(CallGraph cg) {
		Descriptor d = Descriptor.findOrCreateUTF8("([Ljava/lang/String;)V");
		Atom name = Atom.findOrCreateUnicodeAtom("main");
		for (Iterator<? extends CGNode> it = cg.getSuccNodes(cg.getFakeRootNode()); it.hasNext();) {
			CGNode n = it.next();
			if (n.getMethod().getName().equals(name) && n.getMethod().getDescriptor().equals(d)) {
				return n;
			}
		}
		Assertions.UNREACHABLE("failed to find main() method");
		return null;
	}

	public static Statement findCallTo(CGNode n, String methodName) {
		IR ir = n.getIR();
		for (Iterator<SSAInstruction> it = ir.iterateAllInstructions(); it.hasNext();) {
			SSAInstruction s = it.next();
			if (s instanceof com.ibm.wala.ssa.SSAAbstractInvokeInstruction) {
				com.ibm.wala.ssa.SSAAbstractInvokeInstruction call = (com.ibm.wala.ssa.SSAAbstractInvokeInstruction) s;
				if (call.getCallSite().getDeclaredTarget().getName().toString().equals(methodName)) {
					com.ibm.wala.util.intset.IntSet indices = ir.getCallInstructionIndices(call.getCallSite());
					com.ibm.wala.util.debug.Assertions.productionAssertion(indices.size() == 1, "expected 1 but got " + indices.size());
					return new com.ibm.wala.ipa.slicer.NormalStatement(n, indices.intIterator().next());
				}
			}
		}
		Assertions.UNREACHABLE("failed to find call to " + methodName + " in " + n);
		return null;
	}

	public static void dumpSlice(Collection<Statement> slice) {
		for (Statement s : slice) {
			System.err.println(s);
		}
	}
	
	public static CGNode findMethod(CallGraph cg, String name) {
		Atom a = Atom.findOrCreateUnicodeAtom(name);
		for (Iterator<? extends CGNode> it = cg.iterator(); it.hasNext();) {
			CGNode n = it.next();
			if (n.getMethod().getName().equals(a)) {
				return n;
			}
		}
		System.err.println("call graph " + cg);
		Assertions.UNREACHABLE("failed to find method " + name);
		return null;
	}
}
