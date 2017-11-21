#!/bin/bash
# Jordan Thayer  2015-05-06T11:03:53-04:00
# Runs haskell int trs -> its converter against all int trs files in a supplied
# or the current directory.

VERBOSE=1
IN=${1-.}
OUT=${2-$(pwd)}
PNAME=intTRStoITS
FAILED="INTTRS-TO-ITS-FAILED"

## Convert functionality
function convert(){
    INTTRS=$1
    FILE=$(basename "$INTTRS")
    FNAME="${FILE%.*}"
    if [ $VERBOSE -ne 0 ]; then
        echo "converting $FILE to $FNAME.its"
        echo "$PNAME < $INTTRS > $OUT/$FNAME"
    fi
    ## we assume the converter has been cabal installed and is in path
    $PNAME < $INTTRS > $OUT/$FNAME.its
    if [ $? -ne 0 ]; then
        if [ $VERBOSE -ne 0] ; then
            echo "Issue translating $INTTRS"
        fi
        echo "$INTTRS" >> $FAILED
    fi
}

## Usage information

## Arg debugging prints
if [ $VERBOSE -ne 0 ] ; then
    echo "Searching for int trs in \"$IN\""
    echo "Dumping generated files to \"$OUT\""
fi

rm $FAILED

## Check that the converter exists.
type $PNAME >/dev/null 2>&1 ||
{ echo >&2 "I require $PNAME but it's not installed.  Aborting.";
  exit 1; }

while read inttrs; do
    convert $inttrs
done < <(find $DIR -iname *.inttrs)
