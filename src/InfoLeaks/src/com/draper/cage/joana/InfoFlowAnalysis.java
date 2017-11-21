package com.draper.cage.joana;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.ibm.wala.ipa.callgraph.pruned.DoNotPrune;
import com.ibm.wala.ipa.cha.ClassHierarchyException;
import com.ibm.wala.util.CancelException;
import com.ibm.wala.util.graph.GraphIntegrity.UnsoundGraphException;

import edu.kit.joana.api.IFCAnalysis;
import edu.kit.joana.api.lattice.BuiltinLattices;
import edu.kit.joana.api.sdg.SDGBuildPreparation;
import edu.kit.joana.api.sdg.SDGConfig;
import edu.kit.joana.api.sdg.SDGInstruction;
import edu.kit.joana.api.sdg.SDGProgram;
import edu.kit.joana.api.sdg.SDGProgramPart;
import edu.kit.joana.ifc.sdg.core.SecurityNode;
import edu.kit.joana.ifc.sdg.core.violations.IViolation;
import edu.kit.joana.ifc.sdg.graph.JoanaGraph;
import edu.kit.joana.ifc.sdg.graph.SDG;
import edu.kit.joana.ifc.sdg.graph.SDGNode;
import edu.kit.joana.ifc.sdg.graph.SDGSerializer;
import edu.kit.joana.ifc.sdg.graph.slicer.graph.building.CallGraphBuilder;
import edu.kit.joana.ifc.sdg.io.dot.JoanaGraph2Dot;
import edu.kit.joana.ifc.sdg.mhpoptimization.MHPType;
import edu.kit.joana.ifc.sdg.util.JavaMethodSignature;
import edu.kit.joana.util.Stubs;
import edu.kit.joana.wala.core.SDGBuilder.ExceptionAnalysis;
import edu.kit.joana.wala.core.SDGBuilder.PointsToPrecision;
import edu.kit.joana.wala.core.ThreadAwareApplicationLoaderPolicy;

public class InfoFlowAnalysis
{
	/** input program analysis options */
	private final Options opts;
	
	// results from program analysis
	private SDGProgram program = null;
	private IFCAnalysis ana = null;
	private Collection<? extends IViolation<SecurityNode>> result = null;
	
	public static enum JREStub { JAVA_14, JAVA_15, CUSTOM; }
	
	public static class Options
	{
		private String appJar = null;
		/** the class path is either a directory or a jar containing
		 * all the classes of the program which you want to analyze
		 */
		private String appClassPath = null;
		private String entryMethod = null;
		private List<String> sources = null;
		private List<String> sinks = null;
		private boolean writeWholeCallGraph = false;
		private boolean writeWholePDG = false;
		private JREStub jreStub = null;
		/** Specifies jar files of JRE classes, if jreStub == CUSTOM */
		private String jreClassPath = null;
		private boolean computeThreadInterference = false;
		private boolean computeSummaryEdges = false;
		private boolean dotNotPruneLibCalls = false;
		
		public Options appJar(String appJar) {
			this.appJar = appJar; return this;
		}
		public Options appClassPath(String appClassPath) {
			this.appClassPath = appClassPath; return this;
		}
		public Options entryMethod(String entryMethod) {
			this.entryMethod = entryMethod; return this;
		}
		public Options sources(List<String> sources) {
			this.sources = sources; return this;
		}
		public Options sinks(List<String> sinks) {
			this.sinks = sinks; return this;
		}
		public Options writeWholeCallGraph(boolean writeWholeCallGraph) {
			this.writeWholeCallGraph = writeWholeCallGraph; return this;
		}
		public Options writeWholePDG(boolean writeWholePDG) {
			this.writeWholePDG = writeWholePDG; return this;
		}
		public Options jreStub(JREStub jreStub) {
			this.jreStub = jreStub; return this;
		}
		public Options jreClassPath(String jreClassPath) {
			this.jreClassPath = jreClassPath; return this;
		}
		public Options computeThreadInterference(
			boolean computeThreadInterference)
		{
			this.computeThreadInterference = computeThreadInterference;
			return this;
		}
		public Options computeSummaryEdges(
			boolean computeSummaryEdges)
		{
			this.computeSummaryEdges = computeSummaryEdges;
			return this;
		}
		public Options dotNotPrune(boolean dotNotPruneLibCalls) {
			this.dotNotPruneLibCalls = dotNotPruneLibCalls; return this;
		}
		
		public InfoFlowAnalysis build()
			throws ClassHierarchyException, IOException, UnsoundGraphException,
			CancelException
		{
			// check options
			checkNotNull("classPath", this.appClassPath);
			checkNotNull("entryMethod", this.entryMethod);
			checkNotNull("sources", this.sources);
			checkNotNull("sinks", this.sinks);
			checkNotNull("jreStub", this.jreStub);
			if(this.jreStub == JREStub.CUSTOM) {
				checkNotNull("jreClassPath", this.jreClassPath);
			}
			if(!fileExists(appJar)) {
				throw new java.io.FileNotFoundException(appJar);
			}
			return new InfoFlowAnalysis(this);
		}

		private static void checkNotNull(String name, Object value) {
			if(value == null) {
				throw new IllegalArgumentException(
					"No value set to argument: " + name);
			}
		}

		private String[] getJREPaths() {
			if(jreClassPath != null) {
				return jreClassPath.split(File.pathSeparator);
			}
			else return getStubs().getPaths();
		}

		private Stubs getStubs() {
			switch(jreStub) {
				case JAVA_14: return Stubs.JRE_14;
				case JAVA_15: return Stubs.JRE_15;
				case CUSTOM: return Stubs.NO_STUBS;
			}
			throw new IllegalStateException("should not reach here");
		}

		private String getThirdPartyLibsPath() {
			String[] paths = appClassPath.split(File.pathSeparator);
			List<String> thirdPartyLibsPaths = new LinkedList<>();
			for(String path : paths) {
				if(!path.equals(appJar)) { //if path is not application jar
					thirdPartyLibsPaths.add(path);
				}
			}
			return String.join(File.pathSeparator, thirdPartyLibsPaths);
		}
	}
	
	private InfoFlowAnalysis(Options opts)
		throws ClassHierarchyException, IOException, UnsoundGraphException,
		CancelException
	{
		this.opts = opts;
		buildProgramGraph();
	}

	private void buildProgramGraph()
		throws ClassHierarchyException, IOException,
			UnsoundGraphException, CancelException
	{
		String appJar = opts.appJar;

		SDGConfig config = new SDGConfig(appJar,
			opts.entryMethod, opts.getStubs());

		config.setThirdPartyLibsPath(opts.getThirdPartyLibsPath());

		/** compute interference edges to model dependencies between threads
		 * (set to false if your program does not use threads) */
		config.setComputeInterferences(opts.computeThreadInterference);

		/** additional MHP analysis to prune interference edges
		 * (does not matter for programs without multiple threads) */
		config.setMhpType(MHPType.NONE);

		config.setPointsToPrecision(PointsToPrecision.INSTANCE_BASED);

		config.setExceptionAnalysis(ExceptionAnalysis.IGNORE_ALL);

		// prune java library

		config.setExclusions(""
			+ "sun\\/awt\\/.*\n"
			+ "sun\\/swing\\/.*\n"
			+ "apple\\/awt\\/.*\n"
			+ "com\\/apple\\/.*\n"
			+ "java\\/util\\/.*\n"
			+ "java\\/text\\/.*\n"
		);

		if(opts.dotNotPruneLibCalls) {
			config.setPruningPolicy(DoNotPrune.INSTANCE);
		}
		else {
			config.setPruningPolicy(ThreadAwareApplicationLoaderPolicy.INSTANCE);
		}

		config.setComputeSummaryEdges(opts.computeSummaryEdges);

		/** build the PDG */
		SDG sdg = buildSDG(config, opts);
		this.program = new SDGProgram(sdg);

		if(opts.writeWholePDG) {
			// save whole application PDG to disk
			writePDG(appJar, sdg);
		}
		
		if(opts.writeWholeCallGraph) {
			// write call graph of application to disk
			writeCallGraph(appJar, sdg);
		}
	}
	
	private static SDG buildSDG(SDGConfig config, Options opts)
			throws ClassHierarchyException, IOException, UnsoundGraphException, CancelException 
	{
		SDGBuildPreparation.Config bpConfig = makeBuildPreparationConfig(config);
		bpConfig.stubs = opts.getJREPaths();
		return SDGBuildPreparation.compute(System.out, bpConfig);
	}

	private static SDGBuildPreparation.Config makeBuildPreparationConfig(SDGConfig config) {
		JavaMethodSignature mainMethod =
			JavaMethodSignature.fromString(config.getEntryMethod());
		SDGBuildPreparation.Config cfg = new SDGBuildPreparation.Config(
			mainMethod.toBCString(), mainMethod.toBCString(),
			config.getClassPath(), config.getFieldPropagation());
		cfg.thirdPartyLibPath = config.getThirdPartyLibsPath();
		cfg.exceptions = config.getExceptionAnalysis();
		cfg.defaultExceptionMethodState = config.getDefaultExceptionMethodState();
		cfg.pts = config.getPointsToPrecision();
		cfg.accessPath = config.computeAccessPaths();
		cfg.sideEffects = config.getSideEffectDetectorConfig();
		cfg.stubs = config.getStubsPath().getPaths();
		cfg.nativesXML = config.getNativesXML();
		cfg.pruningPolicy = config.getPruningPolicy();
		cfg.exclusions = config.getExclusions();
		cfg.computeAllocationSites = config.computeAllocationSites();
		cfg.cgConsumer = config.getCGConsumer();
		cfg.ctxSelector = config.getContextSelector();
		cfg.ddisp = config.getDynamicDispatchHandling();
		cfg.computeSummaryEdges = config.isComputeSummaryEdges();
		cfg.computeInterference = config.computeInterferences();
		return cfg;
	}
	
	private static void writePDG(String appJar, SDG sdg) throws IOException
	{
		String pdgFileName = getPDGFileName(appJar);
		System.out.println("Writing whole application PDG to file " + pdgFileName);
		FileOutputStream outStr = new FileOutputStream(pdgFileName);
		SDGSerializer.toPDGFormat(sdg, outStr);
		outStr.close();
	}

	private static void writeCallGraph(String appJar, JoanaGraph graph) throws IOException {
		String dotFileName = getDotFileName(appJar);
		System.out.println("Writing whole application call graph to file " + dotFileName);
		JoanaGraph appSubGraph = getApplicationSubGraph(graph);
		JoanaGraph graphForDot =
			CallGraphBuilder.buildEntryGraph(appSubGraph);
		JoanaGraph2Dot.writeDotToFile(graphForDot, dotFileName);
	}

	private static JoanaGraph getApplicationSubGraph(JoanaGraph graph) {
		Collection<SDGNode> allProgramNodes = graph.vertexSet();
		Collection<SDGNode> applicationNodes = new LinkedList<>();
		for(SDGNode node : allProgramNodes) {
			if(ApplicationOnlyChopper.isApplicationNode(node)) {
				applicationNodes.add(node);
			}
		}
		return graph.subgraph(applicationNodes);
	}

	private void buildIFCAnalysis() {
		this.ana = new IFCAnalysis(program);

		/* annotate sources and sinks */
		addSources(program, ana, opts.sources);
		addSinks(program, ana, opts.sinks);

		/* run the analysis */
		this.result = ana.doIFC();
	}

	public void processViolations(ProgramChopper programChopper, ChopPrinter chopPrinter)
		throws IOException
	{
		if(ana == null) { // if have not built IFC Analysis
			buildIFCAnalysis();
		}
		SDG sdg = program.getSDG();
		Collection<ProgramChop> programChops = new LinkedList<>();
		for(IViolation<SecurityNode> violation : result) {
			ProgramChop chop = programChopper.createChop(program, violation);
			if(!chop.getNodes().isEmpty())
				programChops.add(chop);
		}
		if(programChops.isEmpty()) {
			System.out.println("No information flow violations found");
		}
		ProgramChopGrouper pcg = new ProgramChopGrouper(sdg);
		Collection<ProgramChop> importantProgramChops =	pcg.filterImportProgramChops(programChops);
		
		String fileExtension = chopPrinter.getFileExtension();
		String appJar = opts.appJar;
		int fileCtr = 0;
		for(ProgramChop chop : importantProgramChops) {
			String subgraphBaseFileName =
				getSubgraphBaseFileName(sdg, appJar, chop);
			String subgraphFileName = String.format("%s%d.%s",
				subgraphBaseFileName, ++fileCtr, fileExtension);
			String sourceName = nameOfNode(sdg, chop.getSource());
			String sinkName = nameOfNode(sdg, chop.getSink());
			System.out.printf(
				"Found info flow leak from %s to %s%n" +
				"Writing this leak to file: %s%n",
					sourceName, sinkName, subgraphFileName);
			System.out.println();
			chopPrinter.printChop(program, subgraphFileName, chop);
		}
	}

	private static class ProgramChopGrouper
	{
		private final SDG sdg;
		private final HashMap<String, Collection<ProgramChop>> map = new HashMap<>();
		
		public ProgramChopGrouper(SDG sdg) {
			this.sdg = sdg;
		}
		
		private String getId(ProgramChop chop) {
			SDGNode sourceEntryMethod = this.sdg.getEntry(chop.getSource());
			SDGNode sinkEntryMethod = this.sdg.getEntry(chop.getSink());
			String sourceMethodDescriptor = sourceEntryMethod.getBytecodeMethod();
			String sinkMethodDescriptor = sinkEntryMethod.getBytecodeMethod();
			String id = sourceMethodDescriptor + '_' + sinkMethodDescriptor;
			return id;
		}
		
		public void addChop(ProgramChop chop) {
			String id = getId(chop);
			Collection<ProgramChop> duplicateChops;
			if(this.map.containsKey(id)) {
				duplicateChops = this.map.get(id);
			}
			else {
				duplicateChops = new LinkedList<ProgramChop>();
				this.map.put(id, duplicateChops);
			}
			duplicateChops.add(chop);
		}
		
		public Collection<Collection<ProgramChop>> getProgramChopGroups() {
			return this.map.values();
		}
		
		public Collection<Collection<ProgramChop>> groupProgramChops(Collection<ProgramChop> chops)
		{
			for(ProgramChop chop : chops) {
				this.addChop(chop);
			}
			return this.getProgramChopGroups();
		}

		
		private static ProgramChop getSmallestChop(Collection<ProgramChop> chops) {
			ProgramChop smallestChop = null;
			int smallestChopSize = -1;
			for(ProgramChop chop : chops) {
				int chopSize = chop.getNodes().size();
				if(smallestChop == null || chopSize < smallestChopSize) {
					smallestChop = chop;
					smallestChopSize = chopSize;
				}
			}
			return smallestChop;
		}
		
		public Collection<ProgramChop> filterImportProgramChops(Collection<ProgramChop> chops)
		{
			Collection<Collection<ProgramChop>> chopGroups =
				this.groupProgramChops(chops);
			Collection<ProgramChop> importantChops = new LinkedList<>();
			for(Collection<ProgramChop> group : chopGroups) {
				ProgramChop smallestChop = getSmallestChop(group);
				importantChops.add(smallestChop);
			}
			return importantChops;
		}

	}
	
	private static String getSubgraphBaseFileName(
		SDG sdg, String appJar, ProgramChop chop)
	{
		String sourceName = getSourceName(sdg, chop);
		String sinkName = getSinkName(sdg, chop);
		StringBuilder sb = new StringBuilder();
		sb.append(getBaseNameNoExt(appJar));
		sb.append("_Leak_");
		sb.append(sourceName).append('_').append(sinkName).append('_');
		return sb.toString();
	}

	private static String getSourceName(SDG sdg, ProgramChop chop) {
		SDGNode source = chop.getSource();
		return nameOfNode(sdg, source);
	}

	private static String getSinkName(SDG sdg, ProgramChop chop) {
		SDGNode sink = chop.getSink();
		return nameOfNode(sdg, sink);
	}
	
	private static String nameOfNode(SDG sdg, SDGNode node) {
		SDGNode entryMethod = sdg.getEntry(node);
		String descriptor = entryMethod.getBytecodeMethod();
		int parenIndex = descriptor.indexOf('(');
		return descriptor.substring(0, parenIndex);
	}

	public static boolean fileExists(String filepath) {
		return new java.io.File(filepath).isFile();
	}

	private static String getPDGFileName(String appJar) {
		String jarBaseNameNoExt = getBaseNameNoExt(appJar);
		return jarBaseNameNoExt + ".pdg";
	}

	private static String getDotFileName(String appJar) {
		String jarBaseNameNoExt = getBaseNameNoExt(appJar);
		return jarBaseNameNoExt + ".dot";
	}

	private static String getBaseNameNoExt(String filePath) {
		String baseName = new File(filePath).getName();
		int periodIndex = baseName.lastIndexOf('.');
		return (periodIndex == -1) ? baseName :	baseName.substring(0, periodIndex);
	}

	private static void addSources(SDGProgram program, IFCAnalysis ana, List<String> sources) {
		for(String source : sources)
			addSource(program, ana, source);
	}

	private static void addSinks(SDGProgram program, IFCAnalysis ana, List<String> sinks) {
		for(String sink : sinks)
			addSink(program, ana, sink);

	}

	private static void addSource(SDGProgram program, IFCAnalysis ana, String source) {
		System.out.println("source: " + source);
		try {
			ana.addSourceAnnotation(program.getPart(source),
				BuiltinLattices.STD_SECLEVEL_HIGH);
		}
		catch(IllegalArgumentException exc) {
			System.err.println("Source not found: " + source);
		}
	}

	private static void addSink(SDGProgram program, IFCAnalysis ana, String sink) {
		System.out.println("sink: " + sink);
		try {
			ana.addSinkAnnotation(program.getPart(sink),
				BuiltinLattices.STD_SECLEVEL_LOW);
		}
		catch(IllegalArgumentException exc) {
			System.err.println("Sink not found: " + sink);
		}
	}

	public void printAllViolationUnorderedTraces() throws IOException
	{
		List<String> sources = opts.sources;
		List<String> sinks = opts.sinks;
		int ctr = 1;
		for(String source : sources) {
			for(String sink : sinks) {
				printViolationUnorderedTrace(source, sink, ctr++);
			}
		}
		
	}

	private void printViolationUnorderedTrace(
		String source, String sink, int fileCtr)
		throws IOException
	{	
		System.out.printf("Searching for leak from %s to %s",
			source, sink);
		System.out.println();
		SDGProgramPart sourcePart = getProgramPart(source);
		if(sourcePart == null) {
			System.out.printf("No leak found from %s to %s",
				source, sink);
			System.out.println();
			return;
		}
		SDGProgramPart sinkPart = getProgramPart(sink);
		if(sinkPart == null) {
			System.out.printf("No leak found from %s to %s",
				source, sink);
			System.out.println();
			return;
		}
		Set<SDGInstruction> instructionChop =
			program.computeInstructionChop(sourcePart, sinkPart);
		if(instructionChop.isEmpty()) {
			System.out.printf("No leak found from %s to %s",
				source, sink);
			System.out.println();
			return;
		}
		String outputFileName =
			leakBaseFileName(source, sink) + fileCtr + ".txt";
		System.out.printf(
			"Found info flow leak from %s to %s%n" +
			"Writing this leak to file: %s%n",
				source, sink, outputFileName);
		System.out.println();
		PrintWriter out = new PrintWriter(new FileWriter(outputFileName));
		for(SDGInstruction instr : instructionChop) {
			out.println(stringOfInstruction(instr));
		}
		out.flush();
		out.close();
	}
	
	private SDGProgramPart getProgramPart(String partDescriptor) {
		System.out.println("program part descriptor: " + partDescriptor);
		try {
			return program.getPart(partDescriptor);
		}
		catch(IllegalArgumentException exc) {
			System.err.println("Program part not found: " + partDescriptor);
			return null;
		}
	}

	public static String stringOfInstruction(SDGInstruction instr) {
		return instr.getLabel() + " | "
			+ instr.getOwner().getSignature().toBCString() + ":"
			+ instr.getBytecodeIndex();
	}
	
	private String leakBaseFileName(String sourceDescriptor,
			String sinkDescriptor)
	{
		String sourceName = nameOfPart(sourceDescriptor);
		String sinkName = nameOfPart(sinkDescriptor);
		StringBuilder sb = new StringBuilder();
		sb.append(getBaseNameNoExt(opts.appJar));
		sb.append("_LeakUnordered_");
		sb.append(sourceName).append('_').append(sinkName).append('_');
		return sb.toString();
	}
	
	private static String nameOfPart(String partDescriptor) {
		int parenIndex = partDescriptor.indexOf('(');
		return (parenIndex != -1) ?
			partDescriptor.substring(0, parenIndex) :
			partDescriptor;
	}
}
