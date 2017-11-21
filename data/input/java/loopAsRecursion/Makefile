## Define the two arguments that the 'template' expects
AS_JARS := LoopAsRecursion.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

LoopAsRecursion.jar: LoopAsRecursion.class
	jar -cvfe LoopAsRecursion.jar LoopAsRecursion $^
