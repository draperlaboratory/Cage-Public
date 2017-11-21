Instructions for General CAGE Usage
===================================

# Basic Steps

## Run AProVE against your jar

### Invoking AProVE
    The following bash script might be useful:

    ```
    #!/bin/sh
    # A simple script for invoking AProVE

    STRAT_LOC=${STRAT_LOC:- $HOME/stac/cage/data/input/aprove/current.strategy}
    APROVE_LOC=${APROVE_LOC:- $HOME/bin/aprove_jbc_complexity.jar}
    JAVA_HEAP_MAX=2G
    if [ -e $STRAT_LOC ]; then
        if [ -e $APROVE_LOC ]; then
            java -Xmx$JAVA_HEAP_MAX -jar $APROVE_LOC -m wst $@
        else
            echo "Couldn't find aprove.jar.  I expected it to be at $APROVE_LOC".
        exit 1
    fi
    else
      echo "Couldn't find strategy file.  I expected it to be at $STRAT_LOC"
    fi
    ```

    NOTE: This is just a replica of the script located at <PATH>

    Assuming that the following script is in the user's path as aprove, the
    invocation looks like:

    > aprove foo.jar

### Capturing Interim AProVE Output

    Here's a script we find useful for capturing intermediate output from
    AProVE.  It's a simple wrapper around KoAT that copies input files to
    /var/tmp.  AProVE currently cleans up after itself, so this is how we save
    the input for later inspection.

    ```
    #!/bin/bash

    koatBin=/home/jtt3620/stac/kittel-koat/koat.native

    echo "koat $@" >> /var/tmp/koatInvocations
    echo "cp $1 /var/tmp/"
    cp $1 /var/tmp/
    ulimit -t 900 $koatBin $@
    ```

    NOTE: This is just a replica of the script located at <PATH>

### Interpreting AProVE Output

    The final line of the AProVE output tells us whether or not the program under
    investigation halts.  NO is for non-termination, MAYBE is for
    something it can't decide, and otherwise, we get back an O(f(n))
    estimate of the complexity of the function. For non-terminating
    programs, there's clearly a denial of service attack.  For
    polynomial approximations, we must take a closer look at the
    output using KoAT.


## Finding DoS Attacks

### Use KoAT to find CPU Denial-of-Service Attacks

    This is the default mode of operating for KoAT.  Given an ITS, KoAT tells
    us what the run time complexity of that ITS is as a function of it's input.

#### Invoking KoAT

     > ./koat.native foo.koat

#### Interpreting KoAT's output

     KoAT produces a large amount of output, but the most important is the
     penultimate line, which consists of the following:

     Complexity upper bound <value>

     Value comes in two flavors.  Either it is a ?, in which case the program
     doesn't terminate and there's trivially a denial of service.
     Alternatively, a polynomial is returned.  This polynomial describes an
     upper bound on run-time of the program in terms of the arguments to the
     entry-function on the ITS that we fed to KoAT.  From here, a human has to
     map the entry function of the ITS back to JBC, and finally they must
     determine if the given complexity function falls within the
     challenge-specified budget.

### Use KoAT to find Memory Attacks

    Invoking KoAT with the flag `-space-complexity` assigns to each
    function the weight that corresponds to the maximum amount of
    space each invocation uses as a function of the input. Default is
    0, the `new` function assigns 1 cell, and other weights can be
    specified through the interface file.

## Finding Side Channel Attacks
   
   CAGE can find information side channel attacks by timing by using
   AProVE to generate an intermediate representation as an ITS, and
   calling the `detectLeak` program packaged with KoAT. A typical
   example of use requires generating a .koat file, annotating the
   start function arguments which are secret using the annotation
   files, and invoking the tool as

   > ./detectLeak --its foo.koat --annot foo.json



### Generating Annotation Files
    
    The annotation files are JSON files that describe complexity and
    information flow information for a KoAT file. To invoke KoAT on a
    file foo.koat with a given annotation file (typically called foo.json) use the command:

    > ./koat.native -iface-file foo.json foo.koat

    The format of the json annotations is described in ANNOTATIONS.md

### Fast Approximate Solutions
    detectLeak.native returns a fast, approximate solution to the question of
    information leak.  It's sound, but it finds too many issues, because it
    assumes than branches always influence subordinate decisions, even after the
    computation path has passed a post-dominator (i.e. all branches have
    rejoined and the conditionals can no longer be directly observed).

#### running detectLeak

     ./detectLeak --its <itsFile> --annot <annotationFile>

     Where itsFile is generally of the form foo.koat, and the annotationFile
     will be of the form foo.json. In particular, we're looking for secret
     argument positions defined in the foo.json file, so this needs to align
     with the foo.koat file for the analysis to be interesting.

#### Interpreting Output of detectLeak

     The final line of output from detectLeak consists of function symbols
     which can leak information about the secret.  If this list is empty,
     KoAT could find no leaks of information in the program. Otherwise, the
     function symbols in this list represent rewrite heads from which secret
     information could be leaked.

#### Mapping Function Heads Back to Java ByteCode
     
     This can only done by hand for now, and requires a bit of
     knowledge of the inner workings of AProVE.

### Determining Tainted Information Flows to Branches

    The tool for this is taintInference.native.  It lives inside of the Draper
    Labs version of the KoAT tool chain.

#### Building taintInference
     from the kittel-koat directory, run

     make taintInference

#### running taintInference

     > ./taintInference.native --its <itsFile> --annot <annotationFile>

     Where itsFile is generally of the form foo.koat, and the annotationFile
     will be of the form foo.json. In particular, we're looking for secret
     argument positions defined in the foo.json file, so this needs to align
     with the foo.koat file for the analysis to be interesting.

#### Interpreting Output of taintInference

     Taint inference prints out three sets of tables: a table showing
     branch-influence information, an argument influence table showing what
     taint flow can be determined directly, and a final table showing taint flow
     more directly.

##### Branch Influence Table

      Each entry in the branch table is of the form

      > funSym is influenced by branches [ branchInfluences ]

      where funSym is a function symbol of the integer transition
      system. branchInfluences are of the form funSym_index, where funSym is as
      before, and index is a 0-based index of rewrite rules headed by the function
      symbol.  For example, g_2 would represent the third (2 + 1) rewrite rule from g
      onto something else in the input ITS.
      
      When we say that a function is influenced by a particular branch, or set of
      branches, what we are specifically saying is the following.  First, there is
      some path in the ITS from the branch to this function symbol.  Second, there is
      at least one path from the branch to an exit of the ITS that doesn't pass
      through this function symbol (i.e., this function symbol appears before the
      post-dominator of the branch).

##### Argument Influence Tables

      Argument influence tables take the form of
      
      fSym_argPos influences [ ByWayOf:fSym_argPos ]
      
      Where fSym is as before.  argPos is a 0-based index of argument positions in a
      function. So in f(x,y,z), x is f_0, y is f_1, and z is f_2.
      
      ByWayOf:
      
      * PT -- Pass Through, as in f(x,y) -> g(x,y).  x and y are passed through,
              without changes.
      
      * EQ -- Equal, as in f(x,y) -> g(y,x).  x and y appear unchanged, but in
              different places.
      
      * DL -- Delta, as in f(x,y) -> g(x+1,y-1).  x and y are changed before being
            passed along.
      
      * FR -- Fresh, as in f(x,y) -> g(x,z).  z is a fresh variable, not appear on the
              left hand side of the rewrite.  We have to assume that the translation from
              JBC to ITS done by AProVE is hiding computation that relies on both x and y.
      
      * Br -- Branch.  A branch is observable for this rewrite rule, and so arguments
              from that conditional influence this argument position.
      
      And the associated fSym is the position influenced.  Tying it all together,
      
      f_0 [PT: g_0, DL:g_1]
      
      says that f_0 influences both g_0 and g_1.  It is passed directly to g_0, and
      undergoes some change before being passed to g_1.


