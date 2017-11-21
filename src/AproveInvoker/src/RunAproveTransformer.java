import java.io.*;
import java.nio.file.*;
import java.text.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.*;

import com.draper.cage.taint.ApplicationMethodFilter;

import soot.*;
import soot.jimple.toolkits.annotation.purity.*;
import soot.jimple.toolkits.callgraph.*;
import soot.options.*;

public class RunAproveTransformer extends SceneTransformer {

	public static final Logger log;
	private AproveResults aproveResults;

	static {
		log = Logger.getLogger("RunAproveTransformer");
		log.setLevel(Level.FINEST);
		String tstamp = DateFormat.getDateInstance().format(new Date());
		tstamp = tstamp.replace(' ', '-');
		String fileLoc = "RunAprove-" + tstamp + ".log";
		// actually setting up the log files in a try / catch block

		try {
			FileHandler fh = new FileHandler(fileLoc);
			fh.setFormatter(new SimplestFormatter());
			log.addHandler(fh);
			log.info("Capturing output in " + fileLoc);
		} catch(Exception e){
			log.severe("Couldn't set up output for log file: " + e.toString());
		}
	}

	@Override
	protected void internalTransform(String passName, Map<String, String> args) {
		RATOpts opts = new RATOpts(args);
		this.aproveResults = new AproveResults(opts);
		this.aproveResults.recordStartTime();
		new AproveSession(opts).run();
		this.aproveResults.recordEndTime();
	}

	public AproveResults getResults() {
		return this.aproveResults;
	}

	private class AproveSession {

		private Semaphore semaphore;
		private RATOpts opts;

		public AproveSession(RATOpts opts) {
			this.opts = opts;
			int maxNumParallelInvokes = opts.getMaxNumParallelAproveInvokes();
			log.log(Level.INFO,
				String.format("Invoking AProVE (max concurrent invocations = %d)",
						maxNumParallelInvokes));
			this.semaphore = new Semaphore(maxNumParallelInvokes);
		}

		private void run() {

			//--------------- Initialize the state -------------------------
			// debug print
			G.v().out.println(this.opts.toString());

			// Initialize the points-to-analysis. We use the "geometric points-to analysis" which requires
			// initializing the options, then the analysis.
			Map<String,String> sparkOptsMap = new HashMap<String, String>();
			sparkOptsMap.put("set-impl", "hash");
			CallGraph cg = Scene.v().getCallGraph();

			// Initialize the entry point
			SootMethod entryPoint;
			if (this.opts.hasEntryMethod()) {
				Optional<SootMethod> oEntryPoint = this.opts.getEntryPoint();
				if(oEntryPoint.isPresent()) {
					entryPoint = oEntryPoint.get();
				} else {
					throw new IllegalArgumentException("Could not find method "
							+ this.opts.getEntryMethod() + " as an entry point!");
				}
			} else {
				entryPoint = Scene.v().getMainMethod();
			}

			Iterator<Edge> it = cg.edgesOutOf(entryPoint);
			G.v().out.println("CG Edges out of Entry Point:" + entryPoint);
			while (it.hasNext()) {
				G.v().out.println(it.next().getTgt().method().toString());
			}
			//-----------------------------------------------------------------

			//Create the filter which dictates which methods to analyze
			SootMethodFilter applicationMethodFilter = new ApplicationMethodFilter();
			SootMethodFilter methodLoopFilter = new MethodBodyLoopFilter();
			SootMethodFilter unionFilter = new MethodFilterUnion(applicationMethodFilter, methodLoopFilter);
			//Create an iterator for the heads of the call-graph. Currently we give only the main method.
			ArrayList<SootMethod> headsArray = new ArrayList<SootMethod>();
			headsArray.add(entryPoint);

			//Initialize ifa, TODO: we should have a flag for verbosity
			CallGraphTopo cgt =  new CallGraphTopo(cg, unionFilter, headsArray, null, true);
			Stack<SootMethod> analysisOrder = cgt.getAnalysisOrder();
			LinkedList<Thread> aproveInvokingThreads = new LinkedList<Thread>();
			while (!analysisOrder.empty()){
				SootMethod invokeOn = analysisOrder.pop();
				AproveInvocation aproveInvocation =
						new AproveInvocation(invokeOn);
				Thread t = new Thread(aproveInvocation);
				t.start();
				aproveInvokingThreads.add(t);
			}
			// wait for threads to complete
			try {
				for(Thread t : aproveInvokingThreads) {
					t.join();
				}
			}
			catch(InterruptedException exc) {
				throw new RuntimeException(exc);
			}

		}

		private class AproveInvocation implements Runnable
		{
			private final SootMethod method;

			AproveInvocation(SootMethod method) {
				this.method = method;
			}

			@Override
			public void run() {
				try {
					AproveSession.this.semaphore.acquire();
					this.invokeAprove();
					if (this.wantLowerBound()) {
						this.runLoAT();
					}
				}
				catch(Exception e) {
					System.err.println("Exception caught for method " +
							this.method);
				} finally {
					AproveSession.this.semaphore.release();
				}
			}

			private boolean wantLowerBound() {
				return AproveSession.this.opts.runLoATOnFail() && RunAproveTransformer.this.aproveResults.failed(this.method);
			}

			private void runLoAT() throws IOException, InterruptedException {
				Optional<String> its = RunAproveTransformer.this.aproveResults.getAnyITS(this.method);
				if (its.isPresent()) {
					ProcessBuilder pb = new ProcessBuilder("loat", "--limit-smt", "--no-cost-check", "--timeout", Long.toString(AproveSession.this.opts.getLoATTimeout()), its.get());
					Process LoAT = pb.start();
					try (BufferedReader procOut = new BufferedReader(new InputStreamReader(LoAT.getInputStream()))) {
						List<String> loatOut = Util.readAndCloseBuffer(procOut);
						// closing stream procOut signal that we are done reading the stdout from
						// LoAT.  This will allow LoAT.waitFor() to terminate.
						LoAT.waitFor();
						aproveResults.noteLoATResult(method, LoAT.exitValue(), loatOut);
					}
				}
			}

			private void invokeAprove(){
				String thisInvocation = new AproveCmd().get();
				// and now we're just shy the jar that aprove was invoked on
				// run this invocation
				log.info(thisInvocation);
				try {
					Process proc = Runtime.getRuntime().exec(thisInvocation);
					try (BufferedReader procOut = new BufferedReader(new InputStreamReader(proc.getInputStream()));
						 BufferedReader procErr = new BufferedReader(new InputStreamReader(proc.getErrorStream()))){
						//We capture the streams while we echo them
						FileHandler aproveOutput = setupAproveHandler();
						if(aproveOutput != null){
							log.addHandler(aproveOutput);
						}
						List<String> stdout = echoStream(procOut, "StdOut");
						echoStream(procErr, "StdErr");
						// proc.waitFor() needs to appear after reading stdout and stderr
						proc.waitFor();
						int exitValue = proc.exitValue();
						RunAproveTransformer.this.aproveResults.noteAProVEResult(this.method, exitValue, stdout);
						// so we don't get too many filehandles laying around
						if(aproveOutput != null){
							log.removeHandler(aproveOutput);
							aproveOutput.flush();
							aproveOutput.close();
						}
					}
				}catch(Exception e){
					// if anything goes seriously wrong, emit a log message
					log.severe("Error: " + e.toString());

					StringWriter sw = new StringWriter();
					e.printStackTrace(new PrintWriter(sw));
					String stackTraceString = sw.toString();
					log.severe("Exception Stack Trace: " + stackTraceString);
				}
			}

			private class AproveCmd {

				private static final String DefaultOptions =
						"-O java::cage=true "
						+ "-s aprove.Cage.upper "
						+ "-m wst";

				private String cmd = "java ";

				AproveCmd() {
					appendJVMOptions();
					appendAproveLoc();
					appendRTLoc();
					appendDefaultOptions();
					appendAnalysisType();
					appendUserOptions();
					appendSummaryFile();
					appendJSSELoc();
					appendJCELoc();
					appendPathToMethodInfo();
					appendDescriptor();
					appendTimeout();
					appendJar();
				}

				private void appendJVMOptions() {
					if (opts.getLoggingConfig().isPresent()) {
						cmd += "-Djava.util.logging.config.file=" + opts.getLoggingConfig().get() + " ";
					}
					cmd += "-ea -jar ";
				}

				private void appendSpace() {
					this.cmd += " ";
				}

				private void appendAproveLoc() {
					appendSpace();
					this.cmd += AproveSession.this.opts.getAproveLoc();
				}

				private void appendRTLoc() {
					AproveSession.this.opts.getRTLoc().ifPresent(rtLoc -> {
						appendSpace();
						this.cmd += "-O java::path_to_library=" + rtLoc;
					});
				}

				private void appendAnalysisType() {
					appendSpace();
					this.cmd += "-O java::analysis_goal=" + AproveSession.this.opts.getAnalysisType();
				}

				private void appendDefaultOptions() {
					appendSpace();
					this.cmd += DefaultOptions;
				}

				private void appendUserOptions(){
					for(String opt : AproveSession.this.opts.getAproveArgs()){
						this.cmd += " " + opt;
					}
				}

				private void appendSummaryFile() {
					if (AproveSession.this.opts.hasSummariesFile()){
						appendSpace();
						String sumPath = AproveSession.this.opts.getSummariesFile();
						log.info("Using summaries " + sumPath);
						this.cmd += "-O java::path_to_method_summaries=" + sumPath;
					}
				}

				private void appendJSSELoc() {
					AproveSession.this.opts.getJSSELoc().ifPresent(jssePath -> {
						appendSpace();
						log.info("Using JSSE library " + jssePath);
						this.cmd += "-O java::path_to_library=" + jssePath;
					});
				}

				private void appendJCELoc() {
					AproveSession.this.opts.getJCELoc().ifPresent(jcePath -> {
						appendSpace();
						log.info("Using JCE library " + jcePath);
						this.cmd += "-O java::path_to_library=" + jcePath;
					});
				}

				private void appendDescriptor() {
					appendSpace();
					this.cmd += "-q " + Util.getDescriptor(AproveInvocation.this.method);
				}

				private void appendTimeout() {
					appendSpace();
					this.cmd += "-t " + AproveSession.this.opts.getAproveTimeOut();
				}

				private void appendJar() {
					appendSpace();
					this.cmd += getJarPath();
				}

				private void appendPathToMethodInfo() {
					appendSpace();
					String fqn = method.getDeclaringClass().getName() + "." + method.getName();
					this.cmd += "-O java::dump_method_info_to=" + opts.getOutputDir() +"/" + fqn + "_info.txt";
				}

				private String getJarPath(){
					if (AproveSession.this.opts.hasJarLoc()){
						return AproveSession.this.opts.getJarLoc();
					}
					log.info("Diving jar location.");
					// we might want to do this at the outset, but it's side effecting
					// so we have to wait until we're sure that the classpath is already set
					// before we go and ask for it.
					// let's just assume the path to the relevant jar is the process_dir
					List<String> dirs_to_process = Options.v().process_dir();
					// further, it's the first one I guess?
					if (dirs_to_process.size() > 0) {
						return dirs_to_process.get(0);
					}
					throw new RuntimeException("no jar found");
				}

				public String get() {
					return this.cmd;
				}

			}

			private FileHandler setupAproveHandler(){
				FileHandler aproveOutput;
				String mName = sanitize(this.method.getSignature());
				String fName = mName + ".log";
				String outputPath = Paths.get(AproveSession.this.opts.getOutputDir()).resolve(Paths.get(fName)).toString();
				log.info("Capturing output in " + outputPath);
				try{
					// Make sure the output directory exists before we make the files
					File outFile = new File(AproveSession.this.opts.getOutputDir());
					outFile.mkdirs();
					aproveOutput = new FileHandler(outputPath);
					aproveOutput.setFormatter(new SimplestFormatter());
				}catch(Exception e){
					aproveOutput = null;
					log.severe(e.toString());
				}
				return aproveOutput;
			}

			private String sanitize(String s){
				String ret = s;
				ret = ret.replaceAll("<", "");
				ret = ret.replaceAll(">", "");
				ret = ret.replace('/', '.');
				ret = ret.replace(' ', '-');
				return ret;
			}
		}
	}

	private static List<String> echoStream(BufferedReader br, String prefix)
			throws IOException
	{
		List<String> res = new ArrayList<>();
		String line;
		try {
			// logs a line of output to the log file, along with the prefix
			while((line = br.readLine()) != null) {
				res.add(line);
				//the line is where we'd probably look for output strings
				log.info(prefix + " " + line);
			}
		}
		catch(Exception e) {
			// capture I/O failures to the log
			log.severe(e.toString());
		}
		finally {
			// closing stream to ensure Process.waitFor() completes
			br.close();
		}
		return res;
	}
}
