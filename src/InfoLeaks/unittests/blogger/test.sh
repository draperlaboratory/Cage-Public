set -x

CAGE_PRJ=$HOME/Documents/cage_prj
# CAGE_PRJ=/Users/jaltidor/allwork/draper/stac/cage_prj
CAGE=$CAGE_PRJ/cage
BIN_PATH=$CAGE/engagement_1/stac_engagement_1_release_v1.0/Challenge_Programs

# Blogger
BIN=$BIN_PATH/blogger/challenge_program/nanohttpd-javawebserver-2.2.0-SNAPSHOT-jar-with-dependencies.jar
CLASS_PATH=$BIN
MAINCLASS=fi.iki.elonen.JavaWebServer
SOURCES=$CAGE/src/InfoLeaks/unittests/blogger/blogger_sources.txt
SINKS=$CAGE/src/InfoLeaks/unittests/blogger/blogger_sinks.txt

java -jar $CAGE/src/InfoLeaks/InfoLeaks.jar \
  --appjar $BIN \
  --appclasspath $CLASS_PATH \
  --mainclass $MAINCLASS \
  --sources $SOURCES \
  --sinks $SINKS \
  --donotprunelib \
  --format json --format trace --format dot
