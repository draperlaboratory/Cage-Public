## Define the two arguments that the 'template' expects
AS_JARS := RecursiveFib.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

RecursiveFib.jar : RecursiveFib.class
	jar -cvfe RecursiveFib.jar RecursiveFib $^
