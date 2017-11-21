## Make a class
SOOTCP="../../../soot/lib/soot-trunk.jar"

all: DrawCFG.jar

%.class : src/%.java
	javac -cp $(SOOTCP):. -sourcepath src/ -d bin/ $^

DrawCFG.jar: DrawCFG.class
	jar -cfm DrawCFG.jar Manifest.txt -C bin .

clean:
	-rm DrawCallGraph.jar
	-rm bin/*
	-rm *\~

.PHONY: clean
