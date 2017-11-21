#!/bin/bash

java -Xmx2G -ea -jar ~/stac/aprove/dist/lib/aprove.jar -J /usr/lib/jvm/java-7-openjdk-amd64/jre/lib/rt.jar -m wst -q "util.HashTable.findEntry(Ljava/lang/Object;IZ)Lutil/HashTable\$Entry;" -s current.strategy store_key_value.jar
