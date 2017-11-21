#!/bin/bash
## Check to see that java programs state their complexity as part of a
## pre-commit hook.
## Jordan Thayer  2015-04-28T09:39:14-04:00

VERBOSE=0
SAW_COMPLEXITY_ISSUE=0
DIR=${1-.}
CUR=$(pwd)


## Make sure a given file has a complexity annotation
function checkComplexity(){
    if [ $VERBOSE -ne 0 ]; then
        echo "$1 is a java file, checking"
        echo "egrep \"O\(.+\)\" $CUR/$1 &> /dev/null"
    fi
    egrep -i "O\(.+\)|non-terminating|abstract|constant" $CUR/$1 &> /dev/null
    return $?
}


while read jfile; do
    checkComplexity $jfile
    if [ $? -ne 0 ]
    then
        echo "$jfile doesn't have a complexity annotation. Add one."
        SAW_COMPLEXITY_ISSUE=1
    else
        if [ $VERBOSE -ne 0 ]
        then
            echo "$jfile is ok."
        fi
    fi
## Only iterate over the files with a static main -- entry point for AProVE
done < <(find $DIR -iname *.java)

# exit $SAW_COMPLEXITY_ISSUE
exit 0
