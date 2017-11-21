#!/bin/bash

java -jar ../IFlow.jar -cp /usr/lib/jvm/java-7-openjdk-amd64/jre/lib/jce.jar:/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/rt.jar -p wjtp.iflow summaries-file:Trivial-summary.json -p wjtp.iflow entry-method:"<Trivial: void dispatch(int)>" -process-dir Trivial.jar
