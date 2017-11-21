## Define the two arguments that the 'template' expects
AS_JARS := FastExpBigInt.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

FastExpBigInt.jar: FastExpBigInt.class
	jar -cvfe FastExpBigInt.jar FastExpBigInt $^
