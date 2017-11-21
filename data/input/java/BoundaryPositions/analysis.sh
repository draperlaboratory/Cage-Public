#!/bin/bash
# # Invoke AProVE with reasonable flags:
JAR=BoundaryPositionsSlice.jar
METHOD1="BoundaryPositionsSlice.getBoundaryPositions(Ljava/nio/ByteBuffer;[B)[I" ## Constant, assuming loop4 is constant
METHOD2="BoundaryPositionsSlice.loop04(I[BLjava/nio/ByteBuffer;)V" ## n^2, assuming loop05 is n^2 in arg2
METHOD3="BoundaryPositionsSlice.loop05(II[B)V" ## n^2 using summary of loop06
METHOD4="BoundaryPositionsSlice.loop06(II[B)V" ## n^1

java -jar ~/bin/aprove.jar \
     -O java::path_to_library=/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/rt.jar \
     -O java::analyze_complexity=true \
     -O java::summarize_all_library_calls=true \
     -O java::warn_when_creating_default_summaries=true \
     -O java::summarize_unimplemented_native_methods=true \
     -O java::summarize_all_method_calls=true \
     -O java::dump_default_summaries=true \
     -O java::path_to_method_summaries=summaries.json \
     -m wst \
     -q $METHOD2 \
     $JAR


# Get the grimple decompilation:
#java -jar /home/jordan/bin/soot-trunk.jar \
#    -f g \
#    -dump-cfg gb.ule \
#    -process-dir BoundaryPositions.jar
