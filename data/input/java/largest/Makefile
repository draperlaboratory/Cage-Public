## Define the two arguments that the 'template' expects
AS_JARS := Largest.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

Largest.jar : Largest.class
	jar -cvfe Largest.jar Largest $^
