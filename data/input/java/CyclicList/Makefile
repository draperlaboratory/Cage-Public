## Define the two arguments that the 'template' expects
AS_JARS := CyclicList.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

CyclicList.jar: CyclicList.class
	jar -cvfe CyclicList.jar CyclicList $^
