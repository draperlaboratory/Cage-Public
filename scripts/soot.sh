#!/bin/bash
SOOT_LOC=${SOOT_LOC:- $HOME/bin/soot.jar}
JAVA_HEAP_MAX=2G

LIBHOME=${JRE_LIB_HOME:- "/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/"}

if [ -e $SOOT_LOC ]; then
    java -Xmx$JAVA_HEAP_MAX -jar $SOOT_LOC -cp $LIBHOME/rt.jar:$LIBHOME/jce.jar $@
else
    echo "Couldn't find soot.jar.  I expected it to be at $SOOT_LOC".
    exit 1
fi
