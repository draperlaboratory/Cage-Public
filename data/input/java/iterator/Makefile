## Define the two arguments that the 'template' expects
AS_JARS := IteratorExample.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

IteratorExample.jar: IteratorExample.class
	jar -cvfe IteratorExample.jar IteratorExample $^
