## Define the two arguments that the 'template' expects
AS_JARS := AddSecret.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

AddSecret.jar: AddSecret.class
	jar -cvfe AddSecret.jar AddSecret $^
