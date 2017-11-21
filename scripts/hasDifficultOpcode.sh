#!/bin/bash

DIFFICULT=$CAGE_HOME/data/input/aprove/difficult_instructions.txt
FILE=$1

if [[ $FILE == *.jbc ]]; then
    ## grep for any element in difficult against the given jbc file
    grep -f $DIFFICULT $FILE
    result=$?
    if [ $result -eq 0 ]; then
	echo "Found a difficult opcode in $FILE"
    fi
    exit $result
else
    echo "Only intend to look for difficult byte code instructions in jbc files."
    echo "usage : hasDifficultOpcode.sh <path-to-jbc>"
    exit 1
fi
