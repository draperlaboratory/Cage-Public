import java.io.*;
import java.util.*;

import com.draper.cage.taint.flowanalysis.IflowOpts;

import soot.*;

public class RATOpts extends IflowOpts {
  // Paths and environment variables aren't super portable, but then neither is KoAT -- [JTT 06-06-16]
  // here are the things we need to have to be able to invoke AProVE
  private final Optional<String> rtLoc;
  private final Optional<String> jsseLoc;
  private final Optional<String> jceLoc;
  private final String aproveLoc;
  private final String[] aproveArgs;

  private final String outputDir;
  private final int aproveTimeOut;
  private final int maxNumParallelAproveInvokes;
  private final String statsFile;
  private final String libName;
  private final String analysisType;
  private final Optional<String> comment;

  private final boolean runLoATOnFail;
  private final long LoATTimeout;

  // here's the thing we're running aprove on
  private String jarLoc;

  private final String descriptorResultsFile;

  private final Optional<String> loggingConfig;

  public static final String[] flags = {
      "rt-loc",                     // override rt.jar with a custom path
      "jsse-loc",                   // override jsse.jar with a custom path
      "jce-loc",                    // override jce.jar with a custom path
      "jar-loc",                    // what jar are we analyzing?  If not supplied, I'll scumm it from classpath
      "aprove-loc",                 // where is aprove at?
      "java-home",                  // root for jsse, rt, jce if not otherwise supplied
      "out-dir",                    // where do we dump the output from aprove?
      "aprove-args",                // semi-colon delimited list to pass to aprove
      "aprove-timeout",             // aprove invoke time out in seconds
      "aprove-max-parallel-calls",  // max concurrently executing approve calls
      "stats-file",                 // name of CSV file stats are written to
      "lib-name",                   // name of library analyzed
      "run-loat-on-fail",           // run LoAT if we cannot prove an upper bound
      "loat-timeout",               // timeout for LoAT
      "analysis-type",              // analysis type for aprove: can be either time_complexity or space_complexity
      "descriptor-results-file",    // Easier to parse file containing method bytecode descriptors and complexity results
      "comment",                    // just a comment -- useful to keep track of many runs with varying configurations
      "logging-config",             // path to a logging-configuration file for AProVE
  };

  public RATOpts(Map<String,String> opts){
    super(opts);
    this.rtLoc     = getValue(opts, "rt-loc");
    this.jsseLoc   = getValue(opts, "jsse-loc");
    this.jceLoc    = getValue(opts, "jce-loc");

    this.jarLoc    = getOptionValue(opts, "jar-loc", "");
    this.aproveLoc = getAproveLoc(opts);
    this.outputDir = getOptionValue(opts, "out-dir", "./");

    String aproveArgsStr = getOptionValue(opts, "aprove-args", "");
    this.aproveArgs = aproveArgsStr.split(";");

    String aproveTimeOutStr = getOptionValue(opts, "aprove-timeout", "10");
    this.aproveTimeOut = Integer.parseInt(aproveTimeOutStr);

    String maxNumParallelAproveInvokesStr =
        getOptionValue(opts, "aprove-max-parallel-calls", "10");
    this.maxNumParallelAproveInvokes =
        Integer.parseInt(maxNumParallelAproveInvokesStr);

    this.statsFile = getOptionValue(opts, "stats-file", "stats.csv");
    this.libName = getOptionValue(opts, "lib-name", jarLoc);
    this.analysisType = getOptionValue(opts, "analysis-type", "time_complexity");

    this.runLoATOnFail= Boolean.parseBoolean(getOptionValue(opts, "run-loat-on-fail", "false"));
    this.LoATTimeout = Integer.parseInt(getOptionValue(opts, "loat-timeout", "10"));

    this.descriptorResultsFile = getOptionValue(opts, "descriptor-results-file", "descriptor-results.txt");

    this.comment = getValue(opts, "comment");

    this.loggingConfig = getValue(opts, "logging-config");
  }

  private static String getAproveLoc(Map<String,String> opts) {
    String aproveLoc = getOptionValue(opts, "aprove-loc", getDefaultAproveLoc());
    if(aproveLoc == null) {
      throw new RuntimeException(
          "No aprove-loc option nor APROVE_HOME environment variable specified");
    }
	return aproveLoc;
  }

  private static String getDefaultAproveLoc() {
    // First check for a tools directory
    String inHouseAproveJar = "tools/aprove.jar";
    if(isFile(inHouseAproveJar))
      return inHouseAproveJar;
	String aproveDir = System.getenv("APROVE_HOME");
      if(aproveDir != null) {
        String aproveLoc = aproveDir + "/dist/lib/aprove.jar";
        if(isFile(aproveLoc)) {
          return aproveLoc;
        }
		return null;
      }
	return null;
  }

  private static String getOptionValue(Map<String,String> opts,
      String optionName, String defaultValue)
  {
    String specifiedValue = PhaseOptions.getString(opts, optionName);
    return specifiedValue.isEmpty() ?
        defaultValue : specifiedValue;
  }

  private static Optional<String> getValue(Map<String, String> opts, String optionName) {
    String specifiedValue = PhaseOptions.getString(opts, optionName);
    return specifiedValue.isEmpty() ? Optional.empty() : Optional.of(specifiedValue);
  }

  private static boolean isFile(String path) {
    return new File(path).isFile();
  }

  // the has predicates dictate whether a given string was set
  // we could use null / non null defaults for this to avoid the
  // need for a separate test to see if things are set,
  // but I like the tests instead

  public boolean hasJarLoc(){
    return this.jarLoc != null;
  }

  public boolean hasRTLoc(){
    return true;
  }

  public boolean hasJCELoc(){
    return true;
  }

  public boolean hasJSSELoc(){
    return true;
  }

  public boolean hasAproveLoc(){
    return true;
  }

  // a set of getters for settings that we
  // set up in the rat options.  We could make
  // the fields accessible, but I really want them
  // to be read only externally.
  public String getJarLoc(){
    return this.jarLoc;
  }

  public Optional<String> getRTLoc(){
    return this.rtLoc;
  }

  public Optional<String> getJSSELoc(){
    return this.jsseLoc;
  }

  public Optional<String> getJCELoc(){
    return this.jceLoc;
  }

  public String getAproveLoc(){
    return this.aproveLoc;
  }

  public String getOutputDir(){
    return this.outputDir;
  }

  public String[] getAproveArgs(){
    return this.aproveArgs;
  }

  public int getAproveTimeOut(){
    return this.aproveTimeOut;
  }

  public int getMaxNumParallelAproveInvokes(){
    return this.maxNumParallelAproveInvokes;
  }

  public String getStatsFileName(){
	  return this.statsFile;
  }

  public String getLibraryName(){
	  return this.libName;
  }

  public boolean runLoATOnFail() {
	return runLoATOnFail;
  }

  public long getLoATTimeout() {
	return LoATTimeout;
  }

  public String getAnalysisType() {
	  return analysisType;
  }

  public String getDescriptorResultsFileName(){
	  return this.descriptorResultsFile;
  }

  public Optional<String> getComment() {
	  return comment;
  }

  public Optional<String> getLoggingConfig() {
	  return loggingConfig;
  }
}
