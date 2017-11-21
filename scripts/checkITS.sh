#!/bin/bash
# Make sure that the ITS is parsed by koat (ignoring correctness of output)
# Jordan Thayer  2015-05-12T12:38:18-04:00

VERBOSE=1
IN=${1-.}
FAILED="KOAT-ITS-FAILED"
KOAT=koat

function run(){
    ITS=$1
    if [ $VERBOSE -ne 0 ]; then
        echo "Running KoAT against $ITS"
    fi
    $KOAT $ITS >/dev/null 2>&1
    if [ $? -ne 0 ] ; then
        if [ $VERBOSE -ne 0 ] ; then
            echo "KoAT failed on $ITS"
            echo "$ITS" >> $FAILED
        fi
    fi
}

## Usage information

## Arg debugging prints
if [ $VERBOSE -ne 0 ] ; then
    echo "Searching for int trs in \"$IN\""
fi

rm $FAILED

## Check that koat is installed
type $KOAT >/dev/null 2>&1 ||
{ echo >&2 "I require $PNAME but it's not installed.  Aborting.";
  exit 1; }

while read its; do
    run $its
done < <(find $IN -iname "*.its")
