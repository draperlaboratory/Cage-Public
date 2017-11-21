#!/bin/bash
JAR_LOC=AproveInvoker.jar

USAGE="$0 <path-to-jar> <method-to-analyze>"

if [ $# -ne 2 ]; then
    echo "Illegal number of arguments. Expected exactly 2."
    echo $USAGE
    exit 1
else
    echo "Analyzing method $2 found in jar $1"
    java -jar $JAR_LOC -cp /usr/lib/jvm/java-7-openjdk-amd64/jre/lib/jce.jar:/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/rt.jar:$1 $2
    exit $?
fi
