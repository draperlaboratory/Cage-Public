#!/bin/bash
# Prints the number of lines in a byte code file on stdout
# Errors if the input file isn't a byte code file or if somehow wordcount fails

FILE=$1

if [[ $FILE == *.jbc ]]; then
    LINES=$(cat $FILE | wc -l)
    if [ $? -ne 0 ]; then
	echo "Word count failed to count lines in $FILE"
	exit 1
    else
	echo "$LINES"
	exit 0
    fi
else
    echo "Only intend to look for difficult byte code instructions in jbc files."
    echo "usage : SizeOfJBC.sh <path-to-jbc>"
    exit 1
fi
