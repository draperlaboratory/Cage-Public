#!/bin/sh
# A simple script for invoking AProVE

APROVE_LOC=${APROVE_LOC:- $HOME/stac/aprove/dist/lib/aprove.jar}
JAVA_HEAP_MAX=${JAVA_HEAP_MAX:- 2G}

if [ -e $APROVE_LOC ]; then
    java -Xmx$JAVA_HEAP_MAX -cp $APROVE_LOC aprove.CommandLineInterface.JBCFrontendMain --proof $@ > /dev/null
else
    echo "Couldn't find aprove.jar.  I expected it to be at $APROVE_LOC".
    exit 1
fi
