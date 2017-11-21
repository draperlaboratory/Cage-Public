CAGE Installation Instructions for Linux
========================================



# Dependencies

   ## AProVE: Java application that runs with Java 8.

      - Requires Z3 and MiniSat.


   ## KoAT: Compiled Ocaml application.

      - Requires Z3


      - Compiling from source requires the Ocaml compiler and
        ocamlbuild, as well as a recent(ish) version of Make. Depends on
        the ocamlgraph, ocamlnum and yojson libraries.


# Installing AProVE

  No installation instructions: AProVE is only available in binary form.
  This can be downloaded from:

        http://aprove.informatik.rwth-aachen.de/downloads/aprove_jbc_complexity.jar

# Installing Kittel/Koat

  First install Ocaml and opam, either using the packaging system for your OS,
  or by following the instructions here:

  https://opam.ocaml.org/doc/Install.html

  Make sure you have the most recent version of Ocaml:

  > opam init
  > opam update
  > opam upgrade

  Then install the required libraries:

  > opam install batteries ocamlgraph mlgmpidl yojson apron

  Install the ocaml z3 bindings (from here: https://github.com/termite-analyser/z3overlay):
  
  > opam remote add termite https://github.com/termite-analyser/opam-termite.git
  > opam install z3
  

  From the kittel-koat directory, Run the following command:

  > make koat

  The binary koat.native is created in the same directory.

  Similarly, the command

  > make detectLeak

  Builds detectLeak.native.

  For more details, see the INSTALL.md file in the kittel-koat directory.

# Installing Convenience Scripts


   Convenience scripts for invoking aprove, koat, etc, are provided in
   cage/scripts.  Putting these in your path will make invoking the pieces of
   our tool chain much simpler.  We assume that the scripts are linked to, and
   that the links lack the .sh extension (e.g. the link to aprove.sh is just
   aprove).
