# Using AProVE #

Basic Invocation
================

The basic pattern for invoking AProVE (say, packaged as `aprove.jar`)
is the following:

> java -jar aprove.jar -m wst MyJar.jar

This usually needs to be accompanied by several options, which can be
listed by using the option

> -0 java::help

Note that the jar file needs to be accompanied by a manifest
specifying the main class, otherwise the invocation needs to be passed
the option

> -q MethodDescriptor

To invoke the analysis on a specific method.

Options
=======

We give examples of the most immediately relevant options, with reasonable
default values:

> -O java::path\_to\_library=/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/rt.jar

> -O java::path\_to\_method\_summaries=path/to/summaries.json

> -O java::analysis\_goal=time\_complexity

> -O java::summarize\_all\_library\_calls=true

> -O java::summarize\_unimplemented\_native\_methods=true

> -O java::summarize\_all\_method\_calls=true

> -O java::default\_class\_init\_state=YES



Summaries
=========

Summaries can be hand-written in JSON format to specify the run-time
and output size of data from method calls. An example summary is given
[here](example-summary.json), with `this`, `arg0`, `arg1`,... being the
arguments of the method, in order.
