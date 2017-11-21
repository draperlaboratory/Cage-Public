#! /usr/bin/env sh
set -x

CAGE=`readlink -f ../../../..`
JAVA7_HOME=$CAGE/tools/jdk1.7.0_79
SOOTJAR=$CAGE/tools/soot-trunk.jar
BASELIBS=$JAVA7_HOME/jre/lib/rt.jar:.

ENGAGEMENT=$CAGE/engagement_2/stac_engagement_2_release_v1.0/Challenge_Programs_v2.0

APP_NAME=textcrunchr_1
APP_ROOT=$ENGAGEMENT/$APP_NAME
APP_HOME=$APP_ROOT/challenge_program


BIN_PATH=$CAGE/engagement_1/stac_engagement_1_release_v1.0/Challenge_Programs

# Blogger
BIN=$BIN_PATH/blogger/challenge_program/nanohttpd-javawebserver-2.2.0-SNAPSHOT-jar-with-dependencies.jar

java -jar $CAGE/src/UnbalancedBranchDetection/UnbalancedBranchDetection.jar \
  -soot-class-path $BASELIBS \
  -p jtp.ctrans diffthreshold:500 \
  -process-path $BIN

