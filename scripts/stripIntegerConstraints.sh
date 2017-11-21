#!/bin/sh
# Jordan Thayer  2015-04-30T13:48:29-04:00

# Removes Integer constraints from all lines of an integer term rewrite system
# Doesn't actually think at all about file contents. It's just stripping out
# anything that comes between [], as thats how constraints are set off in
# standard int trs representations


# make sure we got the right number of args at least
if [ $# -ne 1 ]; then
    echo "Usage: ./stipintegerConstraints.sh <file.inttrs>"
    exit 1
fi

# Set up some variables for output and such.
VERBOSE=1
fname=$1
base=${fname%.*}
out=$base.its

if [ $VERBOSE -ne 0 ]; then
    echo "Processing $base to $out"
fi
if [ -e $out ]; then
    echo "I refuse to clobber output file $out. Exiting..."
    exit 1
fi
sed 's/\[[^]]*\]//g' $fname > $out
