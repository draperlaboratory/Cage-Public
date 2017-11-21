CAGE User Guide
================

# Introduction
This document describes how to use the software tools of CAGE
to analyze Java Byte Code (JBC) applications for space and time
complexity vulnerabilities.

CAGE is a collection of software tools rather than
a single software application.
CAGE uses many software tools in its frontend and backend.
Since this document focuses on how to use CAGE, we only describe tools
that need to be invoked by the user to find vulnerabilties.

This guide assumes that the reader has installed the backend tools
that are used by CAGE such as Aprove.
The CAGE installation guide is file `$CAGE/doc/instructions/INSTALL.md`.

A workflow for using CAGE to finding vulnerabilities in JBC of
applications is presented below.
Since we want to apply CAGE to find vulnerabilities in the
applications of the DARPA STAC engagement meetings, we show how
to use CAGE to analyze an example application from the DARPA STAC
engagement 1 meeting:  blogger.
Although the steps presented in this guide are for analyzing blogger,
these steps will show how to analyze arbitrary JBC applications with CAGE.

# Setting environment variables in a template script for running CAGE

In this user guide, environment variables will be presented as
quoted names in all capital letters such as `CAGE`.
`CAGE` denotes the root directory of your clone of [GIT][git] repository
`https://github.com/draperlaboratory/cage`, for example.
A dollar sign right before an environment variable denotes
the value that was set to the environment variable.

To run CAGE, the user will set values to environment variables in a
template script: `$CAGE/scripts/full_analysis_template.sh`.
Environment variables mentioned below are defined in this script.

A copy of that script should be created for the user's platform
and for the input application to be analyzed by CAGE.
That script will run the software tools of CAGE on the input application.

# Aprove Path

CAGE uses the [Aprove][aprove] software tool to infer runtime bounds
of methods of the input application.
Specifically, CAGE depends on branch `jbc-complexity` of [GIT][git]
repository `https://github.com/aprove-developers/aprove`.
In the script running the software tools of CAGE, variable
`APROVE_H0ME` should be set to the root directory of local clone of
this GIT repo.

    APROVE_H0ME=$HOME/Documents/cage_prj/aprove

# Directory structure of Engagement meeting applications

This section describes the contents and structure of the directories
Engagement 1 meeting applications.

The path of the root directory containing all of the
Engagement 1 applications is stored in environment
variable `ENGAGEMENT`:

    ENGAGEMENT=$CAGE/engagement_1/stac_engagement_1_release_v1.0/Challenge_Programs

Each application is in a child directory of `ENGAGEMENT`.
The name of the child directory is the name of the application.
The blogger application is in folder `$ENGAGEMENT/blogger`.

    APP_NAME=blogger
    APP_ROOT=$ENGAGEMENT/$APP_NAME

In this case, the name of the application is `blogger`.
The application name is denoted `APP_NAME`.
After running software tool `AproveInvoker`,
`$APP_NAME` will appear in file `$ENGAGEMENT/stats.csv`,
a file that stores statistics about analysis results over the
engagement applications.
Variable `APP_ROOT` stores the path to the application folder.

Each application folder contains the following:

1. `$APP_ROOT/description.txt`:
   Text file describing the application, the application's
   inputs and outputs, and how to run the application.

2. `$APP_ROOT/questions`:
   Directory containing vulnerability questions to answer about
   the application.

3. `$APP_ROOT/challenge_program`:
   Directory storing binaries, scripts, information for running the
   application.

# Defining environment variable `APP_HOME`

The scripts running the engagement applications set environment
variable `APP_HOME`.
Those scripts do not define `APP_ROOT`.
However, they set the value of `APP_HOME` to
same path as `$APP_ROOT/challenge_program`.
`APP_HOME` is used in those scripts for setting the `CLASSPATH`
environment variable.
We also define `APP_HOME` to make it easier to copy the value
of `CLASSPATH` from those script to our script for running
CAGE:

    APP_HOME=$APP_ROOT/challenge_program

We describe how to set the classpath in later in this guide in
Section
[Java CLASSPATH of input application](#java_classpath_of_input_application).

# Application jar file of input application

Variable `APP_JAR` stores the application jar file of the input application.
The application jar file of the input application is the jar file containing
the main class.
`APP_JAR` setting for blogger:

    $APP_HOME/nanohttpd-javawebserver-2.2.0-SNAPSHOT-jar-with-dependencies.jar

# Main class of input application

Variable `MAIN_CLASS` stores the the main class of the input
application.
The main class is the Java class containing the main method or
entry method of the input application.
`MAIN_CLASS` setting for blogger:

    MAIN_CLASS=fi.iki.elonen.JavaWebServer

## How CAGE software tools use main class information

The main class is an optional
[Soot][soot] argument of CAGE tool `AproveInvoker`;
this argument is specified with flag `-main-class`.
If this argument is not specified, `AproveInvoker` tries to guess the
main class.
`AproveInvoker` may guess the wrong the main class.
This happened for STAC engagement application `textcrunchr_1`.
Guessing the wrong main class may decrease the precision of the
taint analysis performed by `AproveInvoker`.
Therefore, we recommend specifying the main class to `AproveInvoker`.

The main class is required input to CAGE tool `InfoLeaks`.

## Main class of a STAC engagement application

Since we want to apply CAGE to find vulnerabilities in the
applications of the DARPA STAC engagement meetings, we describe how
to find the main class of these application.

The main class can be determined from the script that runs
the engagement application.
In the case of blogger, script `$APP_HOME/run.sh` runs
the blogger application.
It runs the main class of `$APP_JAR`.
Blogger's application jar contains a manifest file or a file
named `MANIFEST.mf`, which states the main class of the jar file.

Not all engagement application jar files contain a manifest file.
This is the case for engagement application `textcrunchr_1`.
The script running `textcrunchr_1` is file
`$APP_HOME/bin/textcrunchrhost_1`.
The last command in that script runs the `textcrunchr_1`
application jar.
It specifies that the main class is `com.cyberpointllc.stac.host.Main`.

# Java CLASSPATH of input application

Variable `APP_LIBS` stores the list of directories and jar files
to use in the input application's [classpath][classpath].
The classpath can be determined from the scripts running
the engagement application.
In the case of blogger where no classpath is specified,
the classpath should be set to just the application jar file,
`$APP_JAR`:

    APP_LIBS=$APP_JAR

In the script running engagement application `textcrunchr_1`,
the application's classpath is set to environment variable `CLASSPATH`.
The value to set `CLASSPATH` references variable `APP_HOME`.
Since our script running CAGE also sets `APP_HOME`, for most
engagement applications, we can set the value of `APP_LIBS` to
be the same value set to `CLASSPATH` in the scripts running
engagement applications.

Note that after variable `APP_LIBS` is set to the input application's
classpath, we define variable `LIBS`:

    LIBS=$APP_LIBS:$JAVA7_HOME/jre/lib/rt.jar:$JAVA7_HOME/jre/lib/jce.jar

`LIBS` is the classpath used by `AproveInvoker`.
`APP_LIBS` is the classpath used by `InfoLeaks`.
`LIBS` is set to the current value of `APP_LIBS` appended with
the jar files used by the Java Runtime Environment (JRE).
`AproveInvoker` uses [Soot][soot], which needs to know which
JRE classes to assume.
Adding JRE jar files to the classpath tells Soot (and hence
`AproveInvoker`) the JRE used by the input application.


# Method summaries of input application

Create a custom summaries file for the input application.
Copy file `$CAGE/data/summaries/summaries.json`
to file `$APP_ROOT/summaries.json`:

    cp $CAGE/data/summaries/summaries.json $APP_ROOT/summaries.json

The above command will create the initial version of the
summaries file for the input application.
The summaries file is used by `AproveInvoker`.
It helps improve the accuracy of the results generated by `AproveInvoker`.
The summaries files contain algorithmic time complexity information
for methods that are difficult for `AproveInvoker` to analyze.
This file may be updated by the user as he learns more
precise runtime bounds of methods of the input application.
The user can determine more precise runtime bounds by manually
reading the code of the input application.
A disassembler is software for reading code of binary files.
A disassembler is in the CAGE repository:
`$CAGE/tools/jd-gui-1.4.0.jar`.


# Timeout for Aprove Invocation

Variable `APROVE_TIMEOUT` is a setting that affects the runtime
performance of  `AproveInvoker`.
`AproveInvoker` calls `Aprove` on each *tainted* method of the
input application.
Tainted methods are methods that could potentially process user
input.

`APROVE_TIMEOUT` stores the timeout or maximum number of
seconds allowed for an invocation of `Aprove` on a method.
After the run time of an invocation exceeds `APROVE_TIMEOUT`,
that invocation is killed.
If the invocation is killed for a method, the analysis result
for the analyzed method is recorded as `Killed`.
 
# Maximum number of simultaneous Aprove Invocations

Each invocation of Aprove is run independently of another.
Hence, each invocation can be run in parallel.
We run some invocations in parallel to speed up performance.
We limit the number of parallel invocations to avoid out of
memory errors and to avoid thread starvation with the
`APROVE_TIMEOUT` specified.
The maximum number of parallel calls is set to variable
`MAX_PARALLEL_APROVE_CALLS`.

# Run Time Complexity Analysis on the input application

After setting the values specified in the preceding
sections, you are now able to run `AproveInvoker` on
the input application.
The steps performed in the next section can be performed
while `AproveInvoker` is running.
`AproveInvoker` may take a while to complete execution
depending on the size of the input application.

Just execute your custom version of script
`$CAGE/scripts/full_analysis_template.sh`
after performing the steps described
above to run `AproveInvoker` on the input application.

# Input sources of input application

Running cage tool `InfoLeaks` requires a list of variables
declared in the JBC of the input application.
This list of Java variables specifies which variables store
user input in the input application.
The variables storing user input should be recorded in a file
named `$APP_ROOT/sources.txt`.

Determining this list requires using a disassembler to view
the code of the application jar file.
Code of the input application can be read using disassembler
`$CAGE/tools/jd-gui-1.4.0.jar`.

## Syntax of sources and sinks

Sources and sinks are declarations defined in the JBC of the
input application.
Declarations are members of Java classes such as fields and
methods.
Declarations are specified using Java's
[descriptor syntax][java-descriptors].
The descriptor of a member captures the member's type signature
and fully-qualified name.
An example descriptor used to specify a source method in engagement
application `textcrunchr_1` is below

<pre><code>
com.cyberpointllc.stac.textcrunchr.InputPathHandler.handleInputPath(Ljava/lang/String;)Ljava/util/List;
</code></pre>

This descriptor specifies the method named `handleInputPath` in
class `com.cyberpointllc.stac.textcrunchr.InputPathHandler` that
takes in an instance of `java.lang.String` as an argument and returns
an instance of `java.util.List`.

Formal arguments of a method can also be specified as a source or sink.
To specify which formal argument of a method, add the suffix
`->pN` right after the method's descriptor, where `N` is the index
of the argument.
For non-static methods, the implicit `this` argument is considered
the 0th argument and specified as `p0`.
For instance, the `this` argument of method `handleInputPath` is
specified in the code block below.

<pre><code>
com.cyberpointllc.stac.textcrunchr.InputPathHandler.handleInputPath(Ljava/lang/String;)Ljava/util/List;->p0
</code></pre>

To specify the `java.lang.String` argument of method `handleInputPath`,
substitute `p1` for `p0` in the code block above.

Also, to specify the return value of method `handleInputPath`,
substitute `exit` for `p0` in the code block above.

## Standalone JOANA GUI application

The syntax of sources and sinks can also be determined using
the Standalone JOANA GUI application.
The JOANA GUI application can be invoked by running jar file
`$CAGE/tools/joana.ui.ifc.wala.console.jar`.
Running this GUI application is *not* required to run the CAGE tools.
It is not invoked by any of the CAGE tools.
This tool is provided only as a reference for better understanding
of the [JOANA][joana] software tool.
This tool is described in detail in [this paper][joana-paper].

### Source/Sink Syntax in JOANA GUI application

We briefly list the steps to perform in JOANA GUI application window
to see examples of the syntax for specifying sources and sinks.

1. Select button `Browse` to choose the jar file of the input application.
2. Select the entry method (main method of main class) in the drop-down
   box labeled `Entry method:`.
3. Select button `Build` to build the System Dependence Graph of the
   input application.
4. Near the top of the window, select the `Annotate` tab to see
   the tree listing the Java packages and classes defined in the
   input application.
   Clicking a package node in the tree will expand the node to
   list the classes in the package.
   Clicking a class node will expand the node to list the members
   (e.g. fields and methods) in the class.
   Clicking a method node will expand the node to list formal arguments
   of the method, the exit node of a method, etc.
5. Select a declaration that you wish to annotate as a source or sink.
   Clicking buttons `Source` or `Sink` will cause that declaration to
   be printed in the console window near the bottom of the GUI window.
   The text in the console window can be highlighted, copied, and
   pasted to a text file.

# Generate an Aprove Results File

Running `AproveInvoker` on the input application will generate
file `$APP_ROOT/aprove_results.txt`.
This file contains the list of tainted methods in the input
application and those methods worst case runtime bounds.
This file is used for determining which methods are
potentially vulnerable to time complexity
(denial of service) attacks.

# Optional Step: Improve Runtime Bounds and Remove False Positives

The user may modify the runtime bounds listed in
`$APP_ROOT/aprove_results.txt` to more precise bounds.
More precise runtime bounds may be found after examining
the methods with the disassembler.
This helps eliminate false positives from the list of
sinks/potentially vulnerable methods.

# Run Information Flow Control analysis on the input application

`InfoLeaks` performs info flow control analysis to determine
which sinks can be potentially influenced by sources/user input.
To run `InfoLeaks`, uncomment the command starting
with `java -jar $CAGE/src/InfoLeaks/InfoLeaks.jar`.
The command for invoking `InfoLeaks` is below:

<pre><code>
java -jar $CAGE/src/InfoLeaks/InfoLeaks.jar \
            --appjar $APP_JAR \
            --appclasspath $APP_LIBS \
            --jreclasspath $JAVA7_HOME/jre/lib/rt.jar \
            --mainclass $MAIN_CLASS \
            --sources $APP_ROOT/sources.txt \
            --sinks $APP_ROOT/aprove_results.txt \
            --format dot
</code></pre>


`InfoLeaks` determines if there is an information flow from
a source to a sink.
Each detected flow is called a *violation*.
For each violation, `InfoLeaks` will calculate the
*program chop*.
The program chop is the statements executed in the input
application's runtime trace when performing the violation;
this trace is called the violation's runtime trace.

The program chop is calculated using an over-approximate
static analysis of the input application.
So there may be some statements in the chop that are not
actually executed in violation's runtime trace.

`InfoLeaks` will generate a file representing the program chop,
for each violation found.
The format of the file is specified with the argument to
command-line option `--format`.
Currently, this option accepts four different values:
`dot`, `json`, `trace`, and `unordered`;
these options specify to generate a [DOT][dot] file,
a [JSON][json] file, and a text file respectively.
Command-line option `--format` is not a required argument.
The default value for `--format` is `dot`.

Command-line option `--format` can be specified multiple
times to generate multiple types of files for a single
invocation of `InfoLeaks`.
Passing arguments `--format json --format trace` to `InfoLeaks`
will cause `InfoLeaks` to generate both JSON and text files
representing the violations found, for example.

## TRACE Text file contents

Passing arguments `--format trace` to `InfoLeaks` will also cause
`InfoLeaks` to generate a text file.
The TRACE text file will contain the violation flow trace or the key
sequence of statements executed that causes the information in
a source to leak to a sink.
Each statement is annotated with the following

1. The method that the statement occurs in.
2. The *bytecode index* of the statement in the owning method.
   The owning method of a statement is the method that contains
   that statement.
   Note that statements are in [Joana][joana]'s
   single static assignment (SSA) form.
   Hence, the bytecode index of a statement may not be the same
   as the line number of the statement in the disassembler or
   in the original source code version of the input program.

The TRACE example below is generated for application blogger
from when specifying source

    fi.iki.elonen.NanoHTTPD$HTTPSession.uri

and sink

    fi.iki.elonen.URIVerifier.verify(Ljava/lang/String;)Z

Trace generated by `InfoLeaks`:

<pre><code>
v3 = this.uri | fi.iki.elonen.NanoHTTPD$HTTPSession.getUri()Ljava/lang/String;:1
return v3 | fi.iki.elonen.NanoHTTPD$HTTPSession.getUri()Ljava/lang/String;:4
v6 = p1.getUri() | fi.iki.elonen.JavaWebServer.serve(Lfi/iki/elonen/NanoHTTPD$IHTTPSession;)Lfi/iki/elonen/NanoHTTPD$Response;:5
v9 = v4.canServeUri(v6, #(null)) | fi.iki.elonen.JavaWebServer.serve(Lfi/iki/elonen/NanoHTTPD$IHTTPSession;)Lfi/iki/elonen/NanoHTTPD$Response;:11
v7 = p1.substring(#(1)) | fi.iki.elonen.JavaWebServerPlugin.canServeUri(Ljava/lang/String;Ljava/io/File;)Z:2
v11 = v7.replaceAll(#(/), #(.)) | fi.iki.elonen.JavaWebServerPlugin.canServeUri(Ljava/lang/String;Ljava/io/File;)Z:9
v13 = v11.isEmpty() | fi.iki.elonen.JavaWebServerPlugin.canServeUri(Ljava/lang/String;Ljava/io/File;)Z:14
if (v13 != #(0)) goto 35 | fi.iki.elonen.JavaWebServerPlugin.canServeUri(Ljava/lang/String;Ljava/io/File;)Z:17
v18 = v15.verify(v11) | fi.iki.elonen.JavaWebServerPlugin.canServeUri(Ljava/lang/String;Ljava/io/File;)Z:28
</code></pre>

## UNORDERED Text file contents

Passing arguments `--format unordered` to `InfoLeaks` will also
cause `InfoLeaks` to generate a text file with statements of a
violation chop in the same format as described in the previous
section.
Using `--format unordered` may cause statements in the text
file to be printed out of order.
That is, the order of the statements in the text file may
not correspond to the order that the statements would be
executed if running the input application.
Using `--format unordered` may cause `InfoLeaks` to run
faster than using `--format trace`.


## DOT file contents

The DOT file will contain a subgraph of the function call graph.
This graph contain the following:

1. Functions in the program chop representing the violation.
2. Functions in the program chop from the entry method of the
input application to the violation's source.

## JSON file contents

The JSON file will contain the list of methods called in the
violation's program chop.
The order of methods in this list (starting from top to bottom)
is the approximate order that these methods are called in the
violation's runtime trace.
This order cannot be exact because there may conditional
statements in the program chop that affect the order of
execution.

For each method listed in the JSON file, the statements
in the body of the method that are also in the program
chop are also listed.
This allows the user to see all of the statements in
the program chop.

After generating the JSON representation of the violations,
the user can examine these files to see how the input
application calls the potentially vulnerable methods listed
in `$APP_ROOT/aprove_results.txt`.

An example generated JSON file from `InfoLeaks` is below.
The sources and sinks specified for this example are also
below:

1. Contents of `$APP_ROOT/sources.txt`:

    fi.iki.elonen.NanoHTTPD$HTTPSession.uri

2. Contents of `$APP_ROOT/aprove_results.txt`:

    fi.iki.elonen.URIVerifier.verify(Ljava/lang/String;)Z

3. Contents of JSON file generated:

<pre><code>
{
  "metadata": 
  {
    "numNodes": 201,

    "source": 
    {
      "file": "fi\/iki\/elonen\/NanoHTTPD.java",
      "method": "Lfi\/iki\/elonen\/NanoHTTPD$HTTPSession.uri",
      "id": 6615,
      "label": "field uri",
      "type": "Ljava\/lang\/String",
      "operation": "compound",
      "entryMethod": "fi.iki.elonen.NanoHTTPD$HTTPSession.getUri()Ljava\/lang\/String;"
    },
    
    "sink": 
    {
      "file": "fi\/iki\/elonen\/URIVerifier.java",
      "method": "fi.iki.elonen.URIVerifier.verify(Ljava\/lang\/String;)Z",
      "id": 8952,
      "label": "fi.iki.elonen.URIVerifier.verify(java.lang.String)",
      "type": null,
      "operation": "entry",
      "entryMethod": "fi.iki.elonen.URIVerifier.verify(Ljava\/lang\/String;)Z"
    }
  },

  "methods": 
  [
    {
      "file": "fi\/iki\/elonen\/JavaWebServerPlugin.java",
      "method": "fi.iki.elonen.JavaWebServerPlugin.canServeUri(Ljava\/lang\/String;Ljava\/io\/File;)Z",
      "id": 6618,
      "relevantStatements": 
      [
        "v7 = p1.substring(#(1))",
        "v11 = v7.replaceAll(#(\/), #(.))",
        "v13 = v11.isEmpty()",
        "if (v13 != #(0)) goto 35",
        "v15 = new fi.iki.elonen.URIVerifier",
        "v15.<init>()",
        "v18 = v15.verify(v11)",
        "if (v18 == #(0)) goto 39",
        "PHI v19 = #(1), #(0)"
      ]
    },
    ...
  ]
}
</code></pre>

The JSON example above shows that the key method in the information
from source

    fi.iki.elonen.NanoHTTPD$HTTPSession.uri

to sink

    fi.iki.elonen.URIVerifier.verify(Ljava/lang/String;)Z->p1

is

    fi.iki.elonen.JavaWebServerPlugin.canServeUri(Ljava/lang/String;Ljava/io/File;)Z

The relevant statements in the JSON output are in [JOANA][joana]'s
single static assignment (SSA) form.
Code written in SSA form is difficult to understand.
An easier-to-read representation is given by
disassembler `$CAGE/tools/jd-gui-1.4.0.jar`.
This disassembler will present a source code view of the
JBC of the input application.
Below is the disassembler representation of method
`fi.iki.elonen.JavaWebServerPlugin.canServeUri(Ljava/lang/String;Ljava/io/File;)Z`:

<pre><code>
public boolean canServeUri(String uri, File rootDir)
{
  uri = uri.substring(1).replaceAll("/", ".");
  return (uri.isEmpty()) || (new URIVerifier().verify(uri));
}
</code></pre>

## Custom JRE Classpath

The software library JOANA comes with stubs or models of the
classes of the Java Runtime Environment (JRE) for versions 1.4
and 1.5 of Java.
Command line option `--jrestub` of `InfoLeaks` allows specifying
which stub to use.
The default stub used by `InfoLeaks` is Java 1.4.
Command `InfoLeaks.jar --help` will print out the usage and
each command-line options allowed input arguments.

Some input applications use classes from later versions of Java.
Engagement application `snapbuddy_1`, for example, uses a class
from Java 1.6, `com.sun.net.httpserver.HttpExchange`.
Hence, neither of the stubs provided with JOANA would be
sufficient to analyzing `snapbuddy_1`.
Command-line option `--jreclasspath` allows specifying a
classpath to a custom version of JRE classes.
The JRE classpath should include jar file `rt.jar` of a
Java installation.
Java 1.7's runtime environment jar file is available at
`$CAGE/tools/jdk1.7.0_79/jre/lib/rt.jar`.

## Custom Entry Method

Command-line option `--entrymethod` allows specifying a non-main
method as the entry method of the input application.
Note that either option `--entrymethod` or option `--mainclass`
can be specified but not both.

### Entry method influences size of program graph generated

If the only source is a single non-main method, specifying that
method as the entry method may substantially reduce the size of
the SDG (program graph) generated, speed up analysis, and improve
the precision of the flow analysis performed by JOANA.
JOANA only adds declarations to the SDG that are *reachable* from
the entry method.
An edge in the SDG represents a certain type of relationship
(e.g direct data dependence) between nodes in the SDG.
Since the SDG generated may become much smaller, choosing a
non-main method may also prevent `java.lang.OutOfMemoryError`
errors.

### Fixing "Sink not found" errors with a Custom Entry Method

Specifying a non-main entry method may also cause some
`Sink not found` errors to not happen.
A sink may not be found because JOANA only adds declarations to
the SDG that are *reachable* from the entry method.
JOANA may fail to add edges in the SDG for certain type of
program dependencies.
I am not sure which type of dependencies are missed.
It seems certain function callbacks or function calls from threads
launched by some classes in Java package `com.sun.net.httpserver`
are missed.
For example, in engagement application `snapbuddy_1`, sink

    com.cyberpointllc.stac.snapservice.SnapContext.getUrlParam(Ljava/lang/String;)Ljava/lang/String;

is detected if specifying the following method as the entry method

    com.cyberpointllc.stac.webserver.handler.AbstractHttpHandler.handle(Lcom/sun/net/httpserver/HttpExchange;)V

Although the actual entry method of `snapbuddy_1` is method
`com.cyberpointllc.stac.host.Main.main`, JOANA does not detect
the above sink if specifying this main method as the entry method.


## Generation whole application function call graph

Passing command-line option `--callgraph` to `InfoLeaks` will
cause `InfoLeaks` to generate a dot file representing the
function call graph of the entire input application.

[json]: http://www.json.org/
[classpath]: https://docs.oracle.com/javase/tutorial/essential/environment/paths.html
[joana]: http://pp.ipd.kit.edu/projects/joana/
[soot]: https://github.com/Sable/soot
[dot]: https://en.wikipedia.org/wiki/DOT_(graph_description_language)
[aprove]: https://github.com/aprove-developers/aprove
[git]: https://git-scm.com/
[java-descriptors]: https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.3
[joana-paper]: http://pp.ipd.kit.edu/uploads/publikationen/tooljoana2013atps.pdf
