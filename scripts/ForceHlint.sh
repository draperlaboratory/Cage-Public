#!/bin/bash
# Look through a directory containing haskell project(s)
# Make sure everything's been properly linted
# Jordan Thayer  2015-04-23T13:08:20-04:00

## To turn on, simply copy from here to .git/hooks/pre-push

## Set to 1 if you want output about ok files
VERBOSE=0
SAW_LINT_ISSUE=0
DIR=${1-.}

function lintFile ()
{
    hlint $1 &> /dev/null
    if [ $? -ne 0 ] ; then
        echo "$1 needs linting"
        return 1
    else
        if [ $VERBOSE -ne 0 ] ; then
        echo "$1 OK"
        fi
    fi
    return 0
}

while read hfile; do
    lintFile $hfile
    if [ $? -eq 1 ]
    then
        SAW_LINT_ISSUE=1
    fi
done < <(find $DIR -name "*.hs")

if [ $SAW_LINT_ISSUE -eq 1 ] ; then
    echo "Try and lint your code before comitting."
    exit 1
fi
