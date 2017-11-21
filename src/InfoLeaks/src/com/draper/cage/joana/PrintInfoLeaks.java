package com.draper.cage.joana;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Arrays;

import com.ibm.wala.ipa.cha.ClassHierarchyException;
import com.ibm.wala.util.CancelException;
import com.ibm.wala.util.graph.GraphIntegrity.UnsoundGraphException;

import edu.kit.joana.ifc.sdg.util.JavaMethodSignature;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.Parameter;
import com.beust.jcommander.ParameterException;

import com.draper.cage.joana.InfoFlowAnalysis.JREStub;

public class PrintInfoLeaks
{
	
	private static class Options
	{
		@Parameter(names = {"-h", "--help"}, help = true,
			description = "Print this help message and exit")
		private boolean help = false;

		@Parameter(names = {"--appjar"}, required = true,
			description = "Jar file containing main class")
		private String appJar;

		@Parameter(names = {"--appclasspath"}, required = true,
			description = "CLASSPATH to use for application")
		private String appClassPath;

		@Parameter(names = {"--mainclass"}, required = false,
			description = "Fully-qualified name of class containing main method")
		private String mainClass;
		
		@Parameter(names = {"--entrymethod"}, required = false,
			description = "Entry method of application")
		private String entryMethod;

		@Parameter(names = {"--sources"}, required = true,
			description = "File specifying sources")
		private String sourcesFile;
		
		private List<String> sources;

		@Parameter(names = {"--sinks"}, required = true,
			description = "File specifying sinks")
		private String sinksFile;
		
		private List<String> sinks;

		@Parameter(names = {"--jrestub"}, required = false,
			description = "JRE stub to use: JAVA14, JAVA15, or CUSTOM")
		private String jreStubArg = "JAVA14";

		@Parameter(names = {"--jreclasspath"}, required = false,
			description = "JRE classpath; required if given '--jrestub CUSTOM'")
		private String jreClassPath = null;

		private JREStub jreStub = null;

		@Parameter(names = {"--format"}, required = false,
			description = "Format of program chop output (dot, json, trace, unordered)")
		private List<String> outputFormatArgs = new LinkedList<>(Arrays.asList("dot"));

		private List<OutputFormat> outputFormats = null;

		@Parameter(names = {"--callgraph"}, required = false,
			description = "Write whole application call graph to file")
		private boolean writeWholeCallGraph = false;

		@Parameter(names = {"--pdg"}, required = false,
			description = "Write whole application PDG to file")
		private boolean writeWholePDG = false;

		@Parameter(names = {"--computeThreadEdges"}, required = false,
			description = "Generate JOANA interference (thread) edges in program graph")
		private boolean computeThreadInterference = false;

		@Parameter(names = {"--computeSummaryEdges"}, required = false,
			description = "Generate JOANA summary edges in program graph")
		private boolean computeSummaryEdges = false;

		@Parameter(names = {"--donotprunelib"}, required = false,
				description = "Do NOT Prune Library Method Calls")
		private boolean doNotPruneLibCalls = false;

		private static List<OutputFormat> outputFormatsOfArgs(List<String> strs) {
			List<OutputFormat> outputFormats = new LinkedList<>();
			for(String str : strs) {
				str = str.toLowerCase();
				outputFormats.add(outputFormatOfArg(str));
			}
			return outputFormats;
		}

		private static OutputFormat outputFormatOfArg(String arg) {
			for(OutputFormat format : OutputFormat.values()) {
				if(format.toString().toLowerCase().equals(arg))
					return format;
			}
			throw new ParameterException(
				"Unrecognized value passed to --format: " + arg);
		}

		private static JREStub jreStubOfArg(String arg) {
			String argLower = arg.toLowerCase();
			if(argLower.equals("java14")) return JREStub.JAVA_14;
			else if(argLower.equals("java15")) return JREStub.JAVA_15;
			else if(argLower.equals("custom")) return JREStub.CUSTOM;
			throw new ParameterException(
				"Unrecognized value passed to --jrestub: " + arg);
		}
		
		/** Object that parses command-line options/arguments */
		private JCommander jc;
		
		Options() {
			jc = new JCommander();
			jc.addObject(this);
			jc.setProgramName("InfoLeaks.jar");
		}
		
		void parse(String[] args) throws IOException {
			jc.parse(args);
			if(isHelpOptionSpecified()) {
				return;
			}
			outputFormats = outputFormatsOfArgs(outputFormatArgs);
			if(outputFormats == null || outputFormats.isEmpty()) {
				throw new ParameterException(
					"output format value given not recognized: "
						+ outputFormatArgs);
			}
			if(!InfoFlowAnalysis.fileExists(appJar)) {
				throw new ParameterException(
					"Application jar file not found: " + appJar);
			}
			if(!InfoFlowAnalysis.fileExists(sourcesFile)) {
				throw new ParameterException(
					"Sources file not found: " + sourcesFile);
			}
			if(!InfoFlowAnalysis.fileExists(sinksFile)) {
				throw new ParameterException(
					"Sinks file not found: " + sinksFile);
			}
			jreStub = jreStubOfArg(jreStubArg);
			if(jreStub == JREStub.CUSTOM && jreClassPath == null) {
				throw new ParameterException(
					"Custom JRE stub specified but no JRE classpath given");
			}
			if(mainClass == null && entryMethod == null) {
				throw new ParameterException(
					"Neither entry method nor main class are specified");
			}
			if(mainClass != null && entryMethod != null) {
				throw new ParameterException(
					"Both entry method and main class are specified. "
						+ "Only one should be given");
			}
			if(mainClass != null) {
				entryMethod = mainMethodOfClass(mainClass);
			}
			sources = descriptorsOfFile(sourcesFile);
			sinks = descriptorsOfFile(sinksFile);
		}

		void printUsage() { jc.usage();	}

		boolean isHelpOptionSpecified() { return help; }
	}
	
	private static enum OutputFormat {
		JSON,
		DOT,
		TRACE,
		UNORDERED;
	}
	
	public static String mainMethodOfClass(String mainClass) {
		JavaMethodSignature entryMethod =
			JavaMethodSignature.mainMethodOfClass(mainClass);
		return entryMethod.toBCString();
	}

	public static void main(String[] args)
			throws ClassHierarchyException, IOException, UnsoundGraphException, CancelException
	{
		Options opts = new Options();
		try {
			opts.parse(args);
			if(opts.isHelpOptionSpecified()) {
				opts.printUsage();
				System.exit(0);
			}
		}
		catch(ParameterException exc) {
			System.err.println(exc.getMessage());
			opts.printUsage();
			System.exit(1);
		}
		printViolations(opts);

		// unitTest();
	}

	public static void unitTest()
		throws ClassHierarchyException, IOException, UnsoundGraphException, CancelException
	{
		String cageDir = "/home/alachyankar/draper_work/cage";
		String appJar = cageDir 
			+ "/engagement_1/stac_engagement_1_release_v1.0/Challenge_Programs/blogger/challenge_program"
			+ "/nanohttpd-javawebserver-2.2.0-SNAPSHOT-jar-with-dependencies.jar";
		Options opts = new Options();
		opts.appJar = appJar;
		opts.appClassPath = appJar;
		opts.entryMethod = mainMethodOfClass("fi.iki.elonen.JavaWebServer");
		opts.sources = new LinkedList<>();
		opts.sources.add("fi.iki.elonen.NanoHTTPD$HTTPSession.uri");
		opts.sinks = new LinkedList<>();
		opts.sinks.add("fi.iki.elonen.URIVerifier.verify(Ljava/lang/String;)Z->p1");
		opts.outputFormats = new LinkedList<>();
		opts.outputFormats.add(OutputFormat.DOT);
		printViolations(opts);
	}

	public static void printViolations(Options opts)
		throws ClassHierarchyException, IOException,
			UnsoundGraphException, CancelException
	{
		System.out.println("starting to print violations");
		InfoFlowAnalysis printer = new InfoFlowAnalysis.Options()
			.appJar(opts.appJar)
			.appClassPath(opts.appClassPath)
			.entryMethod(opts.entryMethod)
			.sources(opts.sources)
			.sinks(opts.sinks)
			.jreStub(opts.jreStub)
			.jreClassPath(opts.jreClassPath)
			.writeWholePDG(opts.writeWholePDG)
			.writeWholeCallGraph(opts.writeWholeCallGraph)
			.computeThreadInterference(opts.computeThreadInterference)
			.computeSummaryEdges(opts.computeSummaryEdges)
			.dotNotPrune(opts.doNotPruneLibCalls)
			.build();
		for(OutputFormat format : opts.outputFormats) {
			ProgramChopper chopper = null;
			ChopPrinter chopPrinter = null;
			switch(format) {
				case JSON:
					chopper =
						new SortedChopper(
							new ImportantFilteredChopper(
								new AddAppEntryToSourceChopper(opts.entryMethod,
									new ApplicationOnlyChopper(
										new SlicerChopper()))));
					chopPrinter = new ChopPrinterCleanJSON();
					printer.processViolations(chopper, chopPrinter);
					break;
				case DOT:
					chopper =
						new SortedChopper(
							new AddAppEntryToSourceChopper(opts.entryMethod,
								new ApplicationOnlyChopper(
									new SlicerChopper())));
					chopPrinter = new ChopPrinterCallGraphDot();
					printer.processViolations(chopper, chopPrinter);
					break;
				case TRACE:
					chopper = new TraceCleanerChopper(new TraceChopper());
					chopPrinter = new InstructionChopPrinterText();
					printer.processViolations(chopper, chopPrinter);
					break;
				case UNORDERED:
					printer.printAllViolationUnorderedTraces();
					break;
			}
		}
	}

	private static List<String> descriptorsOfFile(String descriptorsFile)
			throws IOException
	{
		BufferedReader br =
			new BufferedReader(new FileReader(descriptorsFile));
		String line = null;
		LinkedList<String> descriptors = new LinkedList<>();
		while((line = br.readLine()) != null) {
			String trimmedLine = line.trim();
			if(!trimmedLine.isEmpty()) {
				// System.out.println(trimmedLine);
				if(trimmedLine.contains("|")) {
					String[] descriptorAndRuntime = trimmedLine.split("\\|");
					// System.out.println(Arrays.deepToString(descriptorAndRuntime));
					String descriptor = descriptorAndRuntime[0];
					String runtime = descriptorAndRuntime[1];
					if (runtime.startsWith("Killed") || runtime.startsWith("Maybe")) {
						descriptors.add(descriptor);
					}
				} else {
					descriptors.add(trimmedLine);
				}
			}
		}
		br.close();
		return descriptors;
	}
}
