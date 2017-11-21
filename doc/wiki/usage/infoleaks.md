# Using InfoLeaks #

Basic invocation
================


InfoLeaks is built as a Joana plugin, so invoking InfoLeaks should be very
similar to invoking Joana.

The general invocation pattern is the following:

> java -jar InfoLeaks.jar --appjar MyJar.jar --appclasspath MyJar.jar
> --mainclass MyMain --sources src.txt --sinks sinks.txt

Soot should invoke the plugin on the appropriate target,
and print the result of the analysis to the output files named on the
standard output.

Various flags can be passed to the application as needed.


Options
=======

Some examples of options for output are

> --format json
> --format trace
> --format dot

For a list of possible options use

> --help

Sources & Sinks Files
=====================

Sources and sinks can be specified line-by-line using the Joana
format. Roughly, a source is a method descriptor followed by extra
information to specify the argument or field which is a source or
sink.

e.g.

`myMethod(LMyClass;)V->p0`

for the first argument of the method `myMethod`.
