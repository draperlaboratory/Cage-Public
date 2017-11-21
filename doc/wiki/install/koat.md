# Installation of KoAT #

Dependencies
============

KoAT has a number of dependencies.

For compiling:

* KoAT is in ocaml.

* Opam is the ocaml package management system, it can be used to
  install most of the dependencies. Opam can be installed directly
  from the package manager on Debian or Ubuntu:
  `apt-get install opam`
  then run
  > opam init
  > opam update
  > opam upgrade

  
* Then run the following commands to install all ocaml (and C++)
  dependencies:
  
  > opam install batteries ocamlgraph mlgmpidl yojson apron
  > opam remote add termite https://github.com/termite-analyser/opam-termite.git
  > opam install z3
  

* KoAT itself requires a recent version of make to be built.

Installation
============

Once the dependencies are installed, simply run make:

> make clean
> make

This will create the executable `koat.native` which should be added
somewhere in the executable path.

More instructions can be found in the [koat folder itself](koat/INSTALL).


Usage
=====

For usage instructions see [here](../usage/koat.md).





