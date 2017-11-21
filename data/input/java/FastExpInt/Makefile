## Define the two arguments that the 'template' expects
AS_JARS := FastExpInt.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

FastExpInt.jar: Exp.class
	jar -cvfe FastExpInt.jar Exp $^
