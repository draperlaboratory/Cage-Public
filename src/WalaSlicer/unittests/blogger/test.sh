set -x

CAGE_PRJ=$HOME/Documents/cage_prj
CAGE=$CAGE_PRJ/cage
BIN_PATH=$CAGE/engagement_1/stac_engagement_1_release_v1.0/Challenge_Programs

# Blogger
BIN=$BIN_PATH/blogger/challenge_program/nanohttpd-javawebserver-2.2.0-SNAPSHOT-jar-with-dependencies.jar

java -jar $CAGE/src/WalaSlicer/WalaSlicer.jar $BIN
