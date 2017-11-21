# Overview of the CAGE tool #

This page provides an overview of the CAGE tool, the software it
provides, and how to install and use these components.


The general purpose of the cage tool is to help detect or exclude
certain vulnerabilities from Java Byte Code, with particular emphasis
on the following vulnerabilities:

* Algorithmic Complexity (AC) vulnerabilities which involve excessive
  resource consumption (time, memory, disk space) from the program
  under analysis.
  
* Side Channel (SC) vulnerabilities, which enable a potential attacker
  to gain unauthorized information by observing the resource usage of the
  program under analysis.


Tools and Software Included
===========================

The CAGE tool is comprised of the following components:


  * **Iflow** A tool designed to track taint and information flow
  throughout the code, based on Soot.
  
  * **InfoLeaks** A similar tool to Iflow, based on Wala + Joana.

  * **AProVE** A tool which analyzes the complexity of Java Byte Code.

  * **AproveInvoker** A tool which enables calling AProVE on individual
  selected methods of the program under scrutiny, and offers various
  summaries of the results of the analysis.

  * **KoAT** A tool that works in the back-end of AProVE and gives a
    concrete complexity bound, when given a low-level representation
    of the execution path as input.
    
    
Installation Instructions
=========================

The installation instructions can be found in the [install](install)
folder. The tools were developed to work in a linux environment. Using
other environments, while possible in theory, is not recommended.


  * **Iflow** Instructions can be found [here](install/iflow.md).
  
  * **InfoLeaks** Instructions can be found [here](install/infoleaks.md).

  * **AProVE** AProVE is released as an already built jar file located
    at tools/aprove.jar, and does not need any further
    installation. For use with the rest of the toolchain though, it's
    best to make note of the file's location.

  * **AproveInvoker** Instructions can be found [here](install/aproveinvoker.md).

  * **KoAT** Instructions can be found [here](install/koat.md).

Usage Instructions
==================



The installation instructions can be found in the [usage](usage) folder.


  * **Iflow** Instructions can be found [here](usage/iflow.md).
  
  * **InfoLeaks** Instructions can be found [here](usage/infoleaks.md).

  * **AProVE** Instructions can be found [here](usage/aprove.md).

  * **AproveInvoker** Instructions can be found [here](usage/aproveinvoker.md).

  * **KoAT** Instructions can be found [here](usage/koat.md).



Developer Notes
===============


Developer notes are available in the [coding-notes](coding-notes)
folder.

It is also possible to obtain javadoc documentation for the java
projects by building them with `ant doc`.
