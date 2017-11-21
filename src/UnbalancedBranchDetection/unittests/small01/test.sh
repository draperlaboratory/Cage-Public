#! /usr/bin/env sh
set -x
CAGE=`readlink -f ../../../..`
JAVA7_HOME=$CAGE/tools/jdk1.7.0_79
SOOTJAR=$CAGE/tools/soot-trunk.jar
BASELIBS=$JAVA7_HOME/jre/lib/rt.jar:.
BINPATH=`readlink -f .`
LIBS=$BINPATH:$BASELIBS
# Blogger
BIN=Small01

#   -p jtp.ctrans diffthreshold:500 \

java -jar $CAGE/src/UnbalancedBranchDetection/UnbalancedBranchDetection.jar \
  -soot-class-path $LIBS \
  -p jtp.ctrans diffthreshold:1 \
  $BIN

