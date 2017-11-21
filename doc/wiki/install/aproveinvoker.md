# Installation of AproveInvoker #

Dependencies
============

* AproveInvoker works with Java 1.8.

* AproveInvoker is designed as a plug-in for
[Soot](https://github.com/Sable/soot), and so depends on the soot
application, which can be found in [soot-trunk.jar](../../../tools/soot-trunk.jar).

* AproveInvoker depends on some code from Iflow, which should be
  installed (see [iflow.md]).

* AproveInvoker also depends on the
[json-simple](http://code.google.com/p/json-simple/) library, which is
also available in /tools/json-simple-1.1.jar.

* Installation works with ant version 1.9.6.

Installation
============

Normally, moving to the [AproveInvoker](../../../src/AproveInvoker) directory and
just type

> ant dist

If successful, a jar file named `AproveInvoker.jar` should exist in that
directory.

Otherwise, you may have to modify the
[build.xml](../../../src/AproveInvoker/build.xml) file and modify the `cage`,
`tools`, `soot_loc` and `json-simple_loc` property values.

Similarly, documentation can be created by running

> ant doc

Usage
=====

For usage instructions see [here](../usage/aproveinvoker.md).

