#!/bin/bash
# COunts the lines in the api file provided with an example
# Prints the line count on stdout, returns 0 if successful, 1 otherwise


DIR=$1
API=APIsUsed.txt
API_COUNT=$(cat $DIR/$API | wc -l)

if [ $? -ne 0 ]; then
    echo "Failed to count lines in API file"
    exit 1
else
    echo $API_COUNT
    exit 0
fi
