set -x

# CAGE_PRJ=$HOME/Documents/cage_prj
CAGE_PRJ=/Users/jaltidor/allwork/draper/stac/cage_prj
CAGE=$CAGE_PRJ/cage
BIN_PATH=$CAGE/engagement_1/stac_engagement_1_release_v1.0/Challenge_Programs

# Blogger
BIN=AddSecret.jar
CLASS_PATH=$BIN
MAINCLASS=AddSecret
SOURCES=sources.txt
SINKS=sinks.txt

java -jar $CAGE/src/InfoLeaks/InfoLeaks.jar \
  --appjar $BIN \
  --classpath $CLASS_PATH \
  --mainclass $MAINCLASS \
  --sources $SOURCES \
  --sinks $SINKS \
  --format json --format txt --format dot
