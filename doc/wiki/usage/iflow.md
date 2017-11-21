# Using Iflow #

Basic invocation
================


Iflow is built as a Soot plugin, so invoking Iflow should be very
similar to invoking Soot, as described e.g. [here](cs.au.dk/~mis/soot.pdf)

The general invocation pattern is the following:

> java -jar IFlow.jar -process-path MyJar.jar -main-class MyClass

Soot should invoke the iflow plugin on the appropriate target,
and print the result of the analysis to the standard output.

Various flags can be passed to the iflow phase as needed. Passing
a flag to the analysis is done using the following form:

> -p wjtp.iflow option-name:option-value

There is another possible pass to run with iflow, which is activated with

> -p jtp.btrans enabled:true

Options
=======




   * `enabled:true` Enables the phase, true by default

   * `summaries-file:my_file.json` Gives the path to the user-provided annotation summary file

   * `entry-method:foo` Specifies the entry method "foo" as the point at which execution starts. WARNING: this may return the wrong method if foo is overloaded!

   * `handle-exceptions:true` Specifies whether or not to take exceptions into account when computing control flow. Enabled by default. WARNING: disabling this option may make analysis unsound!

   * `cg-dot:my_file.dot` Generates a dot file containing the call graph as computed by Soot, then exits.
    
   * `sink-file:sink-file.txt` specifies the path to the set of sink methods that are used as reference for finding tainted calls to sink methods in a body. Must be added for the tainted sink transform to run. 


Summaries
=========

Passing a summaries file enables specifying the taint behavior of
methods. The syntax is outlined in
[example-summary.json](example-summary.json).


Sinks File
==========

The sinks file is simply a file containing a list of sink methods (one
per line) in the Soot method descriptor format.
