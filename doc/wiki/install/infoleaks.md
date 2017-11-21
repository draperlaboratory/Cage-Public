# Installation of InfoLeaks #

Dependencies
============

* InfoLeaks works with Java 1.8.

* InfoLeaks is designed as a plug-in for Joana, and so depends on that
application, which can be found in [../../../tools/joana.api.jar].

* InfoLeaks also depends on the
[jcommander](http://jcommander.org/) library, which is
also available in [jcommander-1.30.jar](../../../tools/jcommander-1.30.jar).

* InfoLeaks also depends on the
[json-simple](http://code.google.com/p/json-simple/) library, which is
also available in [json-simple-1.1.jar](../../../tools/json-simple-1.1.jar).

* Installation works with ant version 1.9.6.

Installation
============

Normally, moving to the [InfoLeaks](../../../src/InfoLeaks) directory and just type

> ant dist

If successful, a jar file named `InfoLeaks.jar` should exist in that
directory.

Otherwise, you may have to modify the
[build.xml](../../../src/InfoLeaks/build.xml) file and modify the
`cage`, `tools`, `joana_loc`. `json-simple_loc` and `jcommander_loc`
property values.

Similarly, documentation can be created by running

> ant doc

Usage
=====

For usage instructions see [here](../usage/infoleaks.md).

