set -x

# CAGE_PRJ=$HOME/Documents/cage_prj
CAGE_PRJ=/Users/jaltidor/allwork/draper/stac/cage_prj
CAGE=$CAGE_PRJ/cage

# snapbuddy_1
APP_HOME=/Users/jaltidor/allwork/draper/stac/cage_prj/stac_engagement_2_release_v1.0/Challenge_Programs_v2.0/snapbuddy_1/challenge_program
LIBS=$APP_HOME/lib/snapbuddy_1.jar:$APP_HOME/lib/commons-io-2.2.jar:$APP_HOME/lib/commons-lang3-3.4.jar:$APP_HOME/lib/httpclient-4.5.1.jar:$APP_HOME/lib/mapdb-2.0-beta8.jar:$APP_HOME/lib/commons-cli-1.3.jar:$APP_HOME/lib/commons-fileupload-1.3.1.jar:$APP_HOME/lib/jline-2.8.jar:$APP_HOME/lib/httpcore-4.4.3.jar:$APP_HOME/lib/commons-codec-1.9.jar:$APP_HOME/lib/servlet-api-2.5.jar
BIN=$APP_HOME/lib/snapbuddy_1.jar
MAINCLASS=com.cyberpointllc.stac.host.Main
SOURCES=$CAGE/src/InfoLeaks/unittests/snapbuddy_1/sources.txt
SINKS=$CAGE/src/InfoLeaks/unittests/snapbuddy_1/sinks.txt

java -jar $CAGE/src/InfoLeaks/InfoLeaks.jar \
  --appjar $BIN \
  --appclasspath $LIBS \
  --jreclasspath $CAGE/tools/jdk1.7.0_79/jre/lib/rt.jar \
  --entrymethod "com.cyberpointllc.stac.webserver.handler.AbstractHttpHandler.handle(Lcom/sun/net/httpserver/HttpExchange;)V" \
  --sources $SOURCES \
  --sinks $SINKS \
  --format json --format trace --format dot
