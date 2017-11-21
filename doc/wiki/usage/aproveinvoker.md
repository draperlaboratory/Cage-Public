# Using AProVE #

Basic Invocation
================

AproveInvoker is a Soot plugin, which invokes AProVE through system
calls.

The basic pattern for invoking AproveInvoker is the following:

> java -jar AproveInvoker.jar -main-class MyMain -process-path
> MyApp.jar -cp MyPath

This usually needs to be accompanied by several options, which we
list below

Options
=======

> -p wjtp.aprove\_invoker aprove-loc:path/to/aprove.jar

> -p wjtp.aprove\_invoker jar-loc:MyJar.jar

> -p wjtp.aprove\_invoker summaries-file:path/to/summaries.json

> -p wjtp.aprove\_invoker out-dir:path/to/output/dir

> -p wjtp.aprove\_invoker java-home:path/to/java/home

> -p wjtp.aprove\_invoker aprove-timeout:120

> -p wjtp.aprove\_invoker aprove-max-parallel-calls:15

Number of parallel threads that invoke AProVE.

> -p wjtp.aprove\_invoker lib-name:/path/to/lib

> -p wjtp.aprove\_invoker analysis-type:"space_complexity"

> -p wjtp.aprove\_invoker descriptor-results-file:path/to/results.txt

