#!/bin/bash
## Call javap -c -p on every class described in a jar file's manifest.
## basically gives you a byte-code dump of an entire jar
## Jordan Thayer  2015-04-27T13:14:36-04:00

if [[ $1 == *.jar ]]; then
    javap -v -c -p -classpath $1 $(jar -tf $1 | grep "class$" | sed s/\.class$//)
else
    echo "Expected a jar file as an argument.  $1's extension seems wrong."
    exit 1
fi
