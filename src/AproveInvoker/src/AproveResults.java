import java.io.*;
import java.text.*;
import java.util.*;
import java.util.Map.*;

import soot.*;


public class AproveResults
{

	private final String statsFileName;
	private final String descriptorResultsFileName;
	private final String comment;

	enum Result {

		ConstantUpper, LinearUpper, QuadraticUpper, PolynomialUpper, Maybe, Killed, Unknown, Crashed,
		ConstantLower, LinearLower, QuadraticLower, PolynomialLower, ExponentialLower, InfiniteLower, NonTerm;

		static Result parse(String runTime) {
			if (isConstantUpper(runTime)) {
				return ConstantUpper;
			} else if (isLinearUpper(runTime)) {
				return LinearUpper;
			} else if (isQuadraticUpper(runTime)) {
				return QuadraticUpper;
			} else if (isPolynomialUpper(runTime)) {
				return PolynomialUpper;
			} else if (isConstantLower(runTime)) {
				return ConstantLower;
			} else if (isLinearLower(runTime)) {
				return LinearLower;
			} else if (isQuadraticLower(runTime)) {
				return QuadraticLower;
			} else if (isPolynomialLower(runTime)) {
				return PolynomialLower;
			} else if (isExponentialLower(runTime)) {
				return ExponentialLower;
			} else if (isInfiniteLower(runTime)) {
				return InfiniteLower;
			} else if (isNonTerm(runTime)) {
				return NonTerm;
			} else if (isMaybeTerminates(runTime)) {
				return Maybe;
			} else if (isAnalysisKilled(runTime)) {
				return Killed;
			} else {
				return Unknown;
			}
		}

		boolean isUpperBound() {
			switch (this) {
				case ConstantUpper:
				case LinearUpper:
				case QuadraticUpper:
				case PolynomialUpper:
					return true;
				default:
					return false;
			}
		}

		private static boolean isConstant(String runTime, String landau) {
			return runTime.contains("WORST_CASE") &&
					runTime.contains(landau + "(1)");
		}

		private static boolean isLinear(String runTime, String landau) {
			return runTime.contains("WORST_CASE") &&
					(runTime.contains(landau + "(n)") || runTime.contains(landau + "(n^1)")) ;
		}

		private static boolean isQuadratic(String runTime, String landau) {
			return runTime.contains("WORST_CASE") &&
					runTime.contains(landau + "(n^2)");
		}

		private static boolean isPolynomial(String runTime, String landau) {
			return runTime.contains("WORST_CASE") &&
					!isConstantUpper(runTime) &&
					!isLinearUpper(runTime) &&
					!isQuadraticUpper(runTime) &&
					runTime.contains(landau + "(n^");
		}

		private static boolean isConstantUpper(String runTime) {
			return isConstant(runTime, "O");
		}

		private static boolean isLinearUpper(String runTime) {
			return isLinear(runTime, "O");
		}

		private static boolean isQuadraticUpper(String runTime) {
			return isQuadratic(runTime, "O");
		}

		private static boolean isPolynomialUpper(String runTime) {
			return isPolynomial(runTime, "O");
		}

		private static boolean isConstantLower(String runTime) {
			return isConstant(runTime, "Omega");
		}

		private static boolean isLinearLower(String runTime) {
			return isLinear(runTime, "Omega");
		}

		private static boolean isQuadraticLower(String runTime) {
			return isQuadratic(runTime, "Omega");
		}

		private static boolean isPolynomialLower(String runTime) {
			return isPolynomial(runTime, "Omega");
		}

		private static boolean isInfiniteLower(String runTime) {
			return runTime.contains("WORST_CASE(INF");
		}

		private static boolean isNonTerm(String runTime) {
			return runTime.contains("NO");
		}

		private static boolean isExponentialLower(String runTime) {
			return runTime.contains("WORST_CASE") &&
					(runTime.contains("EXP") || runTime.contains("^n") || runTime.contains("NON_POLY"));
		}

		private static boolean isMaybeTerminates(String runTime) {
			return runTime.contains("MAYBE");
		}

		private static boolean isAnalysisKilled(String runTime) {
			return runTime.contains("KILLED");
		}

	}

	private final String analyzedLibName;
	private final int timeOutLength;
	private final int maxNumConcurrentThreads;

	private Date startDateTime = null;
	private Date endDateTime = null;
	private final SimpleDateFormat dateFormat =
	    new SimpleDateFormat("yyyy-MM-dd'T'HH:mm");

	private Map<SootMethod, Result> results = new HashMap<>();
	private Map<SootMethod, Result> loatResults = new HashMap<>();
	private Map<SootMethod, Set<String>> itsMap = new HashMap<>();

	public AproveResults(RATOpts opts)
	{
		this.analyzedLibName = opts.getLibraryName();
		this.timeOutLength = opts.getAproveTimeOut();
		this.maxNumConcurrentThreads = opts.getMaxNumParallelAproveInvokes();
		this.statsFileName = opts.getStatsFileName();
		this.descriptorResultsFileName = opts.getDescriptorResultsFileName();
		this.comment = opts.getComment().orElse("");
	}

	/** recordStartTime() should be called before running
	 * the whole program analysis
	 */
	public void recordStartTime() {
	  this.startDateTime = new Date();
	}

  private String getStartTimeStamp() {
    if(this.startDateTime == null) {
      throw new RuntimeException(
        "Method AproveResults.setStartTimeStamp was never invoked");
    }
	return dateFormat.format(this.startDateTime);
  }

  /** recordEndTime() should be called after running
   * the whole program analysis
   */
  public void recordEndTime() {
    this.endDateTime = new Date();
  }

  private String getEndTimeStamp() {
    if(this.endDateTime == null) {
      throw new RuntimeException(
        "Method AproveResults.setEndTimeStamp was never invoked");
    }
	return dateFormat.format(this.endDateTime);
  }

  /** Returns time difference between this.endDateTime and
   * this.startTime in number of seconds
   **/
  private double getTimeLengthOfAnalysis() {
    long endTimeMilliSeconds = this.endDateTime.getTime();
    long startTimeMilliSeconds = this.startDateTime.getTime();
    double numMilliSeconds = endTimeMilliSeconds - startTimeMilliSeconds;
    return numMilliSeconds / 1000;
  }

  private String getTimeLengthOfAnalysisString() {
    return String.format("%.3f", getTimeLengthOfAnalysis());
  }

	public synchronized int getNumMethodsAnalyzed() {
		return results.keySet().size();
	}

	public synchronized long getNumMethodsWithResult(Result r) {
		return results.keySet().stream().filter(x -> results.get(x) == r).count();
	}

	public synchronized long getNumMethodsUpperBound() {
		return results.keySet().stream().filter(x -> results.get(x).isUpperBound()).count();
	}

	public synchronized long getNumNonTrivialMethods() {
		return getNumMethodsAnalyzed() - getNumMethodsWithResult(Result.ConstantUpper);
	}

	public synchronized String getComment() {
		return comment;
	}

	public static String createColumnTitleRow() {
		Object[] titlesInRow = new Object[]{
				"Library Name",
				"Start Time Analysis",
				"End Time Analysis",
				"Time Length of Analysis",
				"Aprove Time Out",
				"Max # of Concurrent Aprove Calls",
				"# of Methods Analyzed by Aprove",
				"# of Trivial Methods",
				"# of Non-Trivial Methods",
				"# of Maybe Terminates Method",
				"# of Analysis Killed Method",
				"# of Successfully (Upper Bound Found) Methods",
				"Comment"
			};
			return join(titlesInRow, ",");
	}

	public synchronized String createStatsCSVRow() {
		Object[] statsInRow = new Object[]{
			analyzedLibName,
			getStartTimeStamp(),
			getEndTimeStamp(),
			getTimeLengthOfAnalysisString(),
			timeOutLength,
			maxNumConcurrentThreads,
			getNumMethodsAnalyzed(),
			getNumMethodsWithResult(Result.ConstantUpper),
			getNumNonTrivialMethods(),
			getNumMethodsWithResult(Result.Maybe),
			getNumMethodsWithResult(Result.Killed),
			getNumMethodsUpperBound(),
			getComment()
		};
		return join(statsInRow, ",");
	}

	private static String join(Object[] strings, String delimeter) {
		return join(Arrays.asList(strings), delimeter);
	}

	private static String join(List<Object> strings, String delimeter) {
		Iterator<Object> itr = strings.iterator();
		StringBuilder resultBuilder = new StringBuilder();
		if(itr.hasNext()) {
			resultBuilder.append(itr.next().toString());
		}
		while(itr.hasNext()) {
			resultBuilder.append(delimeter)
			             .append(itr.next().toString());
		}
		return resultBuilder.toString();
	}

	private void writeResultsToStatsFile() throws IOException {
		File statsFile = getCanonicalFile(statsFileName);
		createParentDirectory(statsFile);
		if(!statsFile.exists()) {
			createNewStatsFile(statsFile);
		}
		appendLineToFile(statsFile, createStatsCSVRow());
	}

	private static File getCanonicalFile(String fileName) throws IOException {
		return new File(fileName).getCanonicalFile();
	}

	private static void createParentDirectory(File file) throws IOException {
		if(file.isDirectory()) {
			throw new IOException(String.format(
				"Cannot write to %s because it is a directory", file));
		}
		File fileDir = file.getParentFile();
		if(!fileDir.isDirectory()) {
			fileDir.mkdirs();
		}
	}

	/** Creates new stats file prefixed with column title row and a new line */
	private static void createNewStatsFile(File statsFile) throws IOException {
		statsFile.createNewFile();
		appendLineToFile(statsFile, createColumnTitleRow());
	}

	private static void appendLineToFile(File file, String line) throws IOException
	{
		PrintWriter out = new PrintWriter(new FileWriter(file, true));
		out.println(line);
		out.close();
	}

	private void writeDescriptorResultsFile() throws IOException {
		File descriptorFile = getCanonicalFile(this.descriptorResultsFileName);
		createParentDirectory(descriptorFile);
		if(!descriptorFile.exists()) {
			descriptorFile.createNewFile();
		}
		writeDescriptorResults(descriptorFile);
	}

	private void writeDescriptorResults(File file) throws IOException {
		PrintWriter out = new PrintWriter(new FileWriter(file));
		for (Entry<SootMethod, Result> pair : results.entrySet()) {
			SootMethod m = pair.getKey();
			Result analysisResult = pair.getValue();
			String methodBytecodeSig = Util.getDescriptor(m);
			String analysisResultStr = analysisResult.toString();
			out.println(methodBytecodeSig + "|" + analysisResultStr);
		}
		out.close();
	}

	public synchronized void noteAProVEResult(SootMethod method, int returnValue, List<String> output) {
		noteRuntime(method, returnValue, output, results);
		if(returnValue == 0){
			Set<String> itss = findITS(output);
			itsMap.put(method, itss);
		} else {
			results.put(method, Result.Crashed);
		}
	}

	public synchronized void noteLoATResult(SootMethod method, int returnValue, List<String> output) {
		Result res = noteRuntime(method, returnValue, output, loatResults);
		RunAproveTransformer.log.info("lower bound for " + method +": " + res);
	}

	private static Result noteRuntime(SootMethod method, int returnValue, List<String> output, Map<SootMethod, Result> resMap) {
		Result res = null;
		if(returnValue == 0){
			Set<Result> ress = findRuntime(output);
			if (containsMultiple(ress)) {
				throw new RuntimeException("Found multiple run times for method: " + method);
			}
			if (ress.isEmpty()) {
				res = Result.Unknown;
			} else for (Result r: ress) {
				res = r;
			}
		} else {
			res = Result.Crashed;
		}
		resMap.put(method, res);
		return res;
	}

  private static boolean containsMultiple(Collection<?> collec) {
    Iterator<?> itr = collec.iterator();
    if(itr.hasNext()) {
      itr.next(); return itr.hasNext();
    }
	return false;
  }

	/** Returns the runtime if found in the argument stream str */
	private static Set<Result> findRuntime(List<String> output){
		Set<Result> runTime = new LinkedHashSet<>();
		// logs a line of output to the log file, along with the prefix
		for (String line: output) {
			//the line is where we'd probably look for output strings
			if (isRunTime(line)) {
				runTime.add(Result.parse(line));
			}
		}
		return runTime;
	}

	private static Set<String> findITS(List<String> output){
		Set<String> itss = new LinkedHashSet<>();
		// logs a line of output to the log file, along with the prefix
		for (String line: output) {
			//the line is where we'd probably look for output strings
			if (isITS(line)) {
				itss.add(line.split(" ")[2]);
			}
		}
		return itss;
	}

	private static boolean isITS(String line) {
		return line.startsWith("Dumped to");
	}

	private static boolean isRunTime(String line) {
		return line.contains("KILLED") ||
				line.contains("MAYBE") ||
				line.contains("NO") ||
				line.contains("WORST_CASE") ||
				line.contains("Terminating");
	}

	private void writeResultsToStdOut() {
		for (Entry<SootMethod, Result> pair : results.entrySet()) {
			System.out.println(pair.getKey() + "\t" + pair.getValue());
		}
	}

	//print all of the results from the analysis
	// the results are stored in a map, with the key being the
	// method descriptor, and the value being the computed complexity
	public void display(){
		writeResultsToStdOut();
		try {
			RunAproveTransformer.log.info("Writing statistics to file " + statsFileName);
			writeResultsToStatsFile();
			RunAproveTransformer.log.info("Writing method bytecode descriptor-analysis result "
					+ "pairs to file " + this.descriptorResultsFileName);
			writeDescriptorResultsFile();
		}
		catch(IOException e) { throw new RuntimeException(e); }
	}

	public synchronized Optional<String> getAnyITS(SootMethod method) {
		return itsMap.get(method).isEmpty() ? Optional.<String>empty() : Optional.of(itsMap.get(method).iterator().next());
	}

	public synchronized boolean failed(SootMethod method) {
		return !results.get(method).isUpperBound();
	}

}
