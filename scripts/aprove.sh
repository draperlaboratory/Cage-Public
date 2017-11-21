#!/bin/sh
# A simple script for invoking AProVE

STRAT_LOC=${STRAT_LOC:- $HOME/stac/cage/data/input/aprove/current.strategy}
APROVE_LOC=${APROVE_LOC:- $HOME/stac/aprove/dist/lib/aprove.jar}
JAVA_HEAP_MAX=${JAVA_HEAP_MAX:- 2G}

if [ -e $STRAT_LOC ]; then
    if [ -e $APROVE_LOC ]; then
        java -ea -Xmx$JAVA_HEAP_MAX -jar $APROVE_LOC -m wst $@
    else
        echo "Couldn't find aprove.jar.  I expected it to be at $APROVE_LOC".
        exit 1
    fi
else
    echo "Couldn't find strategy file.  I expectedit to be at $STRAT_LOC"
fi
