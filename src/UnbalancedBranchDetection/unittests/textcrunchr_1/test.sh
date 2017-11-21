#! /usr/bin/env sh
set -x
CAGE=`readlink -f ../../../..`
JAVA7_HOME=$CAGE/tools/jdk1.7.0_79
SOOTJAR=$CAGE/tools/soot-trunk.jar
BASELIBS=$JAVA7_HOME/jre/lib/rt.jar:.

# Textcrunchr_1
APP_HOME=$HOME/Documents/cage_prj/stac_engagement_2_release_v1.0/Challenge_Programs_v2.0/textcrunchr_1/challenge_program
LIBS=$APP_HOME/lib/textcrunchr_1.jar:$APP_HOME/lib/scrypt-1.4.0.jar:$APP_HOME/lib/commons-compress-1.3.jar:$APP_HOME/lib/commons-io-2.2.jar:$APP_HOME/lib/commons-lang3-3.4.jar:$APP_HOME/lib/httpclient-4.5.1.jar:$APP_HOME/lib/mapdb-2.0-beta8.jar:$APP_HOME/lib/commons-cli-1.3.jar:$APP_HOME/lib/commons-fileupload-1.3.1.jar:$APP_HOME/lib/jline-2.8.jar:$APP_HOME/lib/httpcore-4.4.3.jar:$APP_HOME/lib/commons-logging-1.2.jar:$APP_HOME/lib/commons-codec-1.9.jar
BIN=$APP_HOME/lib/textcrunchr_1.jar

java -jar $CAGE/src/UnbalancedBranchDetection/UnbalancedBranchDetection.jar \
  -soot-class-path $BASELIBS:$LIBS \
  -p jtp.ctrans diffthreshold:500 \
  -process-path $BIN

