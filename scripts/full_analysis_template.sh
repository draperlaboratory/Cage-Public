set -x

# Settings

# CAGE=$HOME/Documents/cage_prj/cage
# ENGAGEMENT=$HOME/Documents/cage_prj/stac_engagement_2_release_v1.0/Challenge_Programs_v2.0
# APROVE_HOME=$HOME/Documents/cage_prj/aprove
# APROVE_TIMEOUT=30
# MAX_PARALLEL_APROVE_CALLS=15

# Input Program Specific Settings

# APP_NAME=textcrunchr_1
APP_ROOT=$ENGAGEMENT/$APP_NAME
APP_HOME=$APP_ROOT/challenge_program

# APP_JAR=$APP_HOME/lib/textcrunchr_1.jar
# APP_LIBS=$APP_HOME/lib/textcrunchr_1.jar:$APP_HOME/lib/scrypt-1.4.0.jar:$APP_HOME/lib/commons-compress-1.3.jar:$APP_HOME/lib/commons-io-2.2.jar:$APP_HOME/lib/commons-lang3-3.4.jar:$APP_HOME/lib/httpclient-4.5.1.jar:$APP_HOME/lib/mapdb-2.0-beta8.jar:$APP_HOME/lib/commons-cli-1.3.jar:$APP_HOME/lib/commons-fileupload-1.3.1.jar:$APP_HOME/lib/jline-2.8.jar:$APP_HOME/lib/httpcore-4.4.3.jar:$APP_HOME/lib/commons-logging-1.2.jar:$APP_HOME/lib/commons-codec-1.9.jar

# MAIN_CLASS=com.cyberpointllc.stac.host.Main

SUMMARIES=$APP_ROOT/summaries.json
APROVE_RESULTS=$APP_ROOT/aprove_results.txt

SOURCES=$APP_ROOT/sources.txt
SINKS=$APROVE_RESULTS

# End of Input Program Specific Settings

JAVA7_HOME=$CAGE/tools/jdk1.7.0_79
LIBS=$APP_LIBS:$JAVA7_HOME/jre/lib/rt.jar:$JAVA7_HOME/jre/lib/jce.jar
STATS_FILE=$ENGAGEMENT/stats.csv

# Invoke Aprove on all methods

# build the option to pass libraries to AProVE
APROVE_LIBS=""
IFS=':' read -ra ADDR <<< "$LIBS"
for i in "${ADDR[@]}"; do
    if [ ! -z $i ]
    then
        APROVE_LIBS="-O;java::path_to_library=$i;$APROVE_LIBS"
    fi
done

LOGGING_CONFIG=`readlink -f ../src/AproveInvoker/logging.properties`

java -jar $CAGE/src/AproveInvoker/AproveInvoker.jar \
    -main-class $MAIN_CLASS \
    -process-path $APP_JAR \
    -cp $LIBS \
    -p wjtp.aprove_invoker logging-config:$LOGGING_CONFIG \
    -p wjtp.aprove_invoker aprove-loc:$APROVE_HOME/dist/lib/aprove.jar \
    -p wjtp.aprove_invoker jar-loc:$APP_JAR \
    -p wjtp.aprove_invoker summaries-file:$SUMMARIES \
    -p wjtp.aprove_invoker out-dir:$APP_HOME \
    -p wjtp.aprove_invoker aprove-timeout:$APROVE_TIMEOUT \
    -p wjtp.aprove_invoker aprove-max-parallel-calls:$MAX_PARALLEL_APROVE_CALLS \
    -p wjtp.aprove_invoker stats-file:$STATS_FILE \
    -p wjtp.aprove_invoker lib-name:$APP_NAME \
    -p wjtp.aprove_invoker descriptor-results-file:$APROVE_RESULTS \
    -p wjtp.aprove_invoker aprove-args:$APROVE_LIBS \

#Generate DOT Graph of info flows from sources to sinks

# java -jar $CAGE/src/InfoLeaks/InfoLeaks.jar \
#   --appjar $APP_JAR \
#   --appclasspath $APP_LIBS \
#   --jreclasspath $JAVA7_HOME/jre/lib/rt.jar \
#   --mainclass $MAIN_CLASS \
#   --sources $SOURCES \
#   --sinks $SINKS \
#   --format dot


# # Invoke Iflow on the whole program
# java -jar $CAGE/src/interproc_flow/IFlow.jar \
#     -process-path $APP_JAR \
#     -main-class $MAIN_CLASS \
#     -cp $LIBS \
#     -p wjtp.iflow summaries-file:$IFLOW_SOURCES \
#     -p wjtp.iflow entry-method:"$IFLOW_ENTRY" \
#     -p jtp.btrans enabled:false

# # Build the call-graph
# java -jar $CAGE/src/interproc_flow/IFlow.jar \
#     -process-path $APP_JAR \
#     -main-class $MAIN_CLASS \
#     -cp $LIBS \
#     -p wjtp.iflow cg-dot:/tmp/$(basename $APP_JAR)-cg.dot

